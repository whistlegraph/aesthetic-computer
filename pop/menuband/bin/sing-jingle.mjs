#!/usr/bin/env node
// sing-jingle.mjs — jeffrey SINGS the Menu Band campaign jingles.
//
// v6 — the REGISTER round + the SCALES teaching singalong:
//   · REGISTER — `--register <semitones>` (default +12) lifts every line
//     that many semitones ABOVE the engine's minimal-|shift| octave fit
//     (jeffrey: "I could be higher octave?"). The formant envelope is never
//     touched (no kermit by construction) and the goalpost conformance is
//     register-aware (f0-linked bands widen/shift with the lift; duration/
//     energy/click gates unchanged). Per-line FALLBACK LADDER: if a line
//     fails the WER gate at the asked register it re-renders at +7 then 0
//     and the most intelligible take wins (ties prefer the higher register);
//     fallbacks are reported per line.
//   · SCALES (menuband-scales) — a spoken frame around a sung two-octave
//     notepat letter ladder (c d e f g a b h i j k l m n up, back down;
//     h..n = notepat's second octave). Spoken lines are placed verbatim
//     (natural TTS, unpitched, level-matched to the sung lines) with
//     whisper word timings for captions; the ladder is explicit-mode with
//     absolute pitches C3..B4 (register 0, octave_opt off — the ladder IS
//     the register) and letter-name lyrics ("see dee ee …") for curated
//     IPA. WER scoring letter-folds both sides so "C-D-E" == "see dee ee".
//   · Engine fixes this round (spinging/lib/sing_line_world.py R6·2-4):
//     final unstressed syllables stop starving ("diminished" → "deman"),
//     real ~100 ms phrase-boundary silence before phrase-initial fricatives
//     ("keys. Sus" → "kisses"), phrase-medial onset bursts sit prouder
//     ("control" → "Troll").
//
// v5 — the DICTION round: consonant time-stretching the way trained choirs
// handle it (round 4's whisper gate was failing on swallowed consonants):
//   · STRETCHED CONSONANTS — the engine (spinging/lib/sing_line_world.py)
//     stretches sustainable consonant runs (~2.0× unvoiced fricatives via
//     WORLD noise, ~1.7× voiced sonorants through the pitch path); plosives
//     never stretch — they get a ~22 ms pre-plosive glottal-set-up gap and
//     a full-strength raw burst (now 1.5×). Vowel onsets stay ON the beat;
//     the stretch is stolen from preceding vowel tails and bridge sustains.
//   · CHOIR GATED TO VOWELS — the self-choir tacets on consonant frames
//     (choirs unify on vowels; the LEAD carries diction), and the engine
//     reports consonant_spans so the mix ducks the BED an extra ~5 dB under
//     every consonant (the plain sidechain ducks least exactly when the
//     lead is quietest — its consonants). De-esser eased 0.4 → 0.15 so the
//     stretched sibilants survive the vocal bus.
//   · Clarity re-renders now also raise cons_stretch_scale (more diction,
//     less air/vibrato) when a line misses the WER gate.
//   · LEAD-ONLY DIAGNOSTIC — each line's choir-less lead stem is also
//     whisper-transcribed (separates diction gains from choir masking);
//     verbatim in the QA sidecar.
//
// v4 — the legato/intelligibility round, per jeffrey's round-3 notes:
//   · LEGATO — phrase grouping now comes from the notation sidecar
//     (punctuation + melody rests ≥ 0.4 s); inside a phrase the engine
//     bridges inter-word gaps (sustained vowel, gliding f0, shallow energy
//     dip) so words stop being islands. Karaoke word windows extend across
//     bridges. QA gate: intra-phrase voicing continuity ≥ 95 %.
//   · VOICED ONSETS — m/n/l/w… notes begin ON the note through the WORLD
//     pitch path ("mac"'s m no longer floats a beat early at spoken pitch).
//   · ALIGNMENT — whisper punctuation tokens no longer smear word ends into
//     trailing silence (the round-3 "synthesizer" axis collapse), final-word
//     repairs may reclaim the line tail, and adjacent word windows can't
//     overlap (the "mac app" double-sing).
//   · WHISPER ROUND-TRIP GATE — every rendered line is transcribed back by
//     whisper-cli; per-line WER vs the lyric must be ≤ 0.25 with every
//     content word present, else the line re-renders with clarity tweaks.
//     The vocal stem is re-transcribed per line and the final mixed reel
//     once more as a smoke check; transcripts land verbatim in the QA
//     sidecar.
//   · US IPA — pronounce.mjs now demands GenAm ("store" /stɔɹ/, not /stɔː/).
//
// v3 (kept) — the spinging engine round. The line-continuous WORLD chain
// lives in spinging/lib (this script is its first caller), grounded in TEXT
// rather than spectra alone. Per line:
//
//   1 · TTS through /api/say (provider "jeffrey", stability 0.55 — identity;
//       lines are cached, never re-spent)
//   2 · whisper-cli -ml 1 word boundaries, reconciled by
//       spinging/lib/align-words.mjs (+ the presplit/rescale/repair guards)
//   3 · CHORAL NOTATION (spinging/lib/notation.mjs): every note gets its
//       syllable underlay + {onset, nucleus, coda} phonemes from curated IPA
//       (Wiktionary → espeak fallback, cached in spinging/cache).
//   4 · spinging/lib/sing_line_world.py — guided alignment (expected
//       consonants anchor burst detection), per-line octave transposition
//       minimizing |f0 shift| from the spoken take, harvest clamped to the
//       real 60–300 Hz range (de-kermit), unstretched vowel onsets with
//       natural-pitch glides, note-edge contour tapers (no pre-transition
//       dip), monotone source maps (no stutter), goalpost-conformed drift/
//       vibrato, airy sustains + a quiet self-choir (angelic), and a
//       percentile-conformance + click-scan QA gate — the line re-renders
//       with adjusted tweaks until it sits inside the reference bands.
//   5 · lines placed at absolute time → out/<slug>-vocal.wav, then a soft
//       reverb halo (spinging/lib/vocal_bus.py) on the vocal bus
//   6 · MASTERED mix: vocal bus (highpass → 3:1 compression → de-ess) ducks
//       the jingle bed, then two-pass ffmpeg loudnorm to -14 LUFS / -1 dBTP
//       → out/<slug>-sung.mp3
//
// Also writes out/<slug>.words.sung.json (never touching any existing
// words.json) — the karaoke caption timings the --sung sims burn in.
//
// Run:  node pop/menuband/bin/sing-jingle.mjs menuband-announce
//       (or menuband-features / menuband-chords / all)
//       --harmony 0.875   0 = fully spoken contour, 1 = perfect pitch lock

import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, mkdirSync, copyFileSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";
import { createHash } from "node:crypto";
import { alignWords } from "../../../spinging/lib/align-words.mjs";
import { buildLineScore, writeLineScore } from "../../../spinging/lib/notation.mjs";
import { sourceCounts } from "../../../spinging/lib/pronounce.mjs";
import { decodeAudioMono } from "../../lib/preview-shared.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const OUT = `${LANE}/out`;
const POP = resolve(LANE, "..");
const REPO = resolve(POP, "..");
const SPINGING = `${REPO}/spinging`;
const VENV_PY = `${POP}/.venv/bin/python`;
const WORLD_HELPER = `${SPINGING}/lib/sing_line_world.py`;
const VOCAL_BUS = `${SPINGING}/lib/vocal_bus.py`;
const GOALPOSTS = `${SPINGING}/cache/goalposts.json`;
const WHISPER_MODEL = `${process.env.HOME}/.whisper-models/ggml-base.en.bin`;
const SAY_URL = "https://aesthetic.computer/api/say";
const STABILITY = 0.55;      // >= 0.5 keeps jeffrey's identity
const SR = 48_000;
// --harmony: contour-retention as a first-class knob (0 = spoken, 1 = lock).
// Round 3 defaults to a ~0.875 lock — "drift more into the perfect harmony".
const hIdx = process.argv.indexOf("--harmony");
const HARMONY = hIdx > 0 ? parseFloat(process.argv[hIdx + 1]) : 0.875;
// --register: semitones ABOVE the minimal-shift octave fit (v6 default +12 —
// "I could be higher octave?"). Lines that fail the WER gate up there fall
// back down the ladder (+7, then 0) and the best take wins.
const rIdx = process.argv.indexOf("--register");
const REGISTER = rIdx > 0 ? parseInt(process.argv[rIdx + 1], 10) : 12;
const REGISTER_FALLBACKS = [7, 0];
const QA_PASSES = 3;         // re-render budget per line (percentile gate)
const CLARITY_PASSES = 2;    // extra re-renders if the whisper WER gate fails
const BRIDGE_MAX_S = 0.45;   // keep in sync with sing_line_world.BRIDGE_MAX_S
const WER_GATE = 0.25;       // per-line whisper round-trip ceiling
const CONTINUITY_GATE = 0.95; // intra-phrase voicing continuity floor

// ── the lyrics ─────────────────────────────────────────────────────────────
// "lead" mode: words consume the jingle's lead notes in order — [word, nSlots]
// where nSlots is the word's syllable count. Totals must equal the lead lane.
// "explicit" mode (chords): every syllable is hand-placed on the triad —
// [word, [[t, dur, midi], …]] with midis already in the baritone register.
const LYRICS = {
  "menuband-announce": {
    mode: "lead", transpose: -24,
    lines: [
      { tts: "Menu Band sings out!",
        words: [["menu", 2], ["band", 1], ["sings", 1], ["out", 1]] },
      { tts: "Right up in your menu bar.",
        words: [["right", 1], ["up", 1], ["in", 1], ["your", 1], ["menu", 2], ["bar", 1]] },
      { tts: "A synthesizer now.",
        words: [["a", 1], ["synthesizer", 4], ["now", 1]] },
      { tts: "Every key plays a note.",
        words: [["every", 2], ["key", 1], ["plays", 1], ["a", 1], ["note", 1]] },
      { tts: "It's out now on the Mac App Store.",
        words: [["it's", 1], ["out", 1], ["now", 1], ["on", 1], ["the", 1], ["mac", 1], ["app", 1], ["store", 1]] },
      { tts: "Menu band dot app!",
        words: [["menu", 2], ["band", 1], ["dot", 1], ["app", 1]] },
    ],
  },
  "menuband-features": {
    mode: "lead", transpose: -24,
    lines: [
      { tts: "Click the note. Piano!",
        words: [["click", 1], ["the", 1], ["note", 1], ["piano", 3]] },
      { tts: "A hundred twenty eight instruments, yeah!",
        words: [["a", 1], ["hundred", 2], ["twenty", 2], ["eight", 1], ["instruments", 3], ["yeah", 1]] },
      { tts: "Type to play.",
        words: [["type", 1], ["to", 1], ["play", 1]] },
      { tts: "Goes fullscreen.",
        words: [["goes", 1], ["fullscreen", 2]] },
      { tts: "Your keys make music for free.",
        words: [["your", 1], ["keys", 1], ["make", 1], ["music", 2], ["for", 1], ["free", 1]] },
      { tts: "The Mac App Store.",
        words: [["the", 1], ["mac", 1], ["app", 1], ["store", 1]] },
    ],
  },
  // Chord hits land at seg.t0 + 0.15, restrikes at seg.t0 + 1.45
  // (render-jingles.mjs chordHit) — phrase fronts sit on the hit, the
  // quality word answers on the restrike, sung on the actual triad tones.
  "menuband-chords": {
    mode: "explicit",
    lines: [
      { tts: "One key. One note.",
        words: [["one", [[0.55, 0.5, 48]]], ["key", [[1.15, 0.5, 48]]],
                ["one", [[1.85, 0.5, 48]]], ["note", [[2.35, 0.5, 48]]]] },
      { tts: "Command. Major.",
        words: [["command", [[3.05, 0.4, 48], [3.45, 0.55, 52]]],
                ["major", [[4.35, 0.45, 55], [4.8, 0.55, 52]]]] },
      { tts: "Option. Minor.",
        words: [["option", [[5.55, 0.4, 45], [5.95, 0.55, 48]]],
                ["minor", [[6.85, 0.45, 52], [7.3, 0.55, 48]]]] },
      { tts: "Command option. Sus two.",
        words: [["command", [[8.05, 0.32, 48], [8.37, 0.33, 50]]],
                ["option", [[8.7, 0.32, 55], [9.02, 0.33, 50]]],
                ["sus", [[9.35, 0.45, 48]]], ["two", [[9.8, 0.55, 50]]]] },
      { tts: "Option control. Diminished.",
        words: [["option", [[10.55, 0.32, 47], [10.87, 0.33, 50]]],
                ["control", [[11.2, 0.32, 53], [11.52, 0.33, 50]]],
                ["diminished", [[11.85, 0.3, 47], [12.15, 0.3, 50], [12.45, 0.45, 53]]]] },
      { tts: "All three keys. Sus four.",
        words: [["all", [[13.05, 0.38, 48]]], ["three", [[13.43, 0.4, 53]]],
                ["keys", [[13.83, 0.45, 55]]],
                ["sus", [[14.35, 0.45, 53]]], ["four", [[14.8, 0.55, 48]]]] },
      { tts: "Every chord under three keys.",
        words: [["every", [[15.4, 0.4, 48], [15.8, 0.4, 52]]],
                ["chord", [[16.25, 0.7, 45]]],
                ["under", [[17.1, 0.35, 53], [17.45, 0.35, 57]]],
                ["three", [[17.95, 0.6, 55]]],
                ["keys", [[18.8, 1.6, 48]]]] },
    ],
  },
  // v6 — the SCALES teaching singalong. A spoken frame around the notepat
  // two-octave letter ladder; timings + pitches come from the shared
  // menuband-scales.score.json sidecar (render-jingles.mjs), so buildLines
  // assembles the words there. `register: 0` — the ladder places its own
  // absolute pitches (C3..B4, bright top, unstrained); octave_opt off.
  "menuband-scales": {
    mode: "scales", register: 0,
    intro: "Here's how to type out the C scale.",
    run: "C, D, E, F, G, A, B. Now sing it!",
    asc: "C. D. E. F. G. A. B. H. I. J. K. L. M. N.",
    desc: "M, L, K, J, I, H, B, A, G, F, E, D, C!",
    outro: "Wanna type it yourself? Try Menu Band.",
  },
};

// notepat letter → letter-NAME word (the lyric the ladder is sung on —
// real dictionary words so pronounce.mjs serves curated GenAm IPA).
const LETTER_WORDS = {
  a: "ay", b: "bee", c: "see", d: "dee", e: "ee", f: "eff", g: "gee",
  h: "aitch", i: "eye", j: "jay", k: "kay", l: "ell", m: "em", n: "en",
};
const LETTER_NAME_SET = new Set(Object.values(LETTER_WORDS));

// ── helpers ────────────────────────────────────────────────────────────────
// Percentile-gate feedback: nudge the engine's tweak knobs toward the
// reference bands (see spinging/lib/vocal_shapes.py conformance()).
function adjustTweaks(tweaks, conf) {
  if (!conf) return;
  const miss = (k) => (conf[k] && conf[k].pass === false ? conf[k] : null);
  let m;
  if ((m = miss("plateau_drift_cents"))) {
    if (m.value > m.hi) { tweaks.drift_scale *= 0.55; tweaks.beta_scale *= 0.75; }
    else tweaks.drift_scale *= 1.6;
  }
  if ((m = miss("onset_glide_ms")) || (m = miss("onset_glide_cents"))) {
    tweaks.glide_scale *= m.value > m.hi ? 0.7 : 1.35;
  }
  if ((m = miss("release_cents"))) {
    if (m.value > m.hi) tweaks.beta_scale *= 0.8;
  }
  if ((m = miss("vib_depth_cents"))) {
    tweaks.vib_depth_scale *= m.value > m.hi ? 0.6 : 1.5;
  }
  if ((m = miss("hf_ratio"))) {
    tweaks.air_scale *= m.value > m.hi ? 0.6 : 1.5;
  }
}

function measureLoudnorm(file) {
  const r = sh("ffmpeg", ["-y", "-i", file, "-af",
    "loudnorm=I=-14:TP=-1.5:LRA=11:print_format=json", "-f", "null", "-"]);
  const m = r.stderr.toString().match(/\{[^{}]*"input_i"[\s\S]*?\}/);
  if (!m) throw new Error(`loudnorm measure pass printed no JSON for ${file}`);
  return JSON.parse(m[0]);
}

const sh = (cmd, args, opts = {}) => {
  const r = spawnSync(cmd, args, { encoding: "utf8", maxBuffer: 256 * 1024 * 1024, ...opts });
  if (r.status !== 0) throw new Error(`${cmd} ${args[0]}: ${r.stderr?.toString().slice(0, 400)}`);
  return r;
};

function writeWavF32(path, samples, sr = SR) {
  const n = samples.length;
  const buf = Buffer.alloc(44 + n * 4);
  buf.write("RIFF", 0); buf.writeUInt32LE(36 + n * 4, 4); buf.write("WAVE", 8);
  buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(3, 20); // float
  buf.writeUInt16LE(1, 22); buf.writeUInt32LE(sr, 24); buf.writeUInt32LE(sr * 4, 28);
  buf.writeUInt16LE(4, 32); buf.writeUInt16LE(32, 34);
  buf.write("data", 36); buf.writeUInt32LE(n * 4, 40);
  for (let i = 0; i < n; i++) buf.writeFloatLE(samples[i], 44 + i * 4);
  writeFileSync(path, buf);
}

async function ttsLine(text, outFile, attempt = 0) {
  if (existsSync(outFile)) return;
  try {
    await ttsLineOnce(text, outFile);
  } catch (e) {
    if (attempt >= 2) throw e;
    console.log(`  … /api/say hiccup (${String(e.message).slice(0, 60)}), retrying`);
    await new Promise((r) => setTimeout(r, 2500 * (attempt + 1)));
    await ttsLine(text, outFile, attempt + 1);
  }
}

async function ttsLineOnce(text, outFile) {
  const res = await fetch(SAY_URL, {
    method: "POST",
    headers: { "Content-Type": "application/json", Origin: "https://aesthetic.computer" },
    body: JSON.stringify({ from: text, provider: "jeffrey", stability: STABILITY }),
    redirect: "follow",
  });
  if (!res.ok) throw new Error(`/api/say ${res.status}: ${(await res.text()).slice(0, 300)}`);
  const ctype = res.headers.get("content-type") || "";
  let buf;
  if (ctype.includes("application/json")) {
    const json = await res.json();
    if (json.audio) buf = Buffer.from(json.audio, "base64");
    else if (json.url) buf = Buffer.from(await (await fetch(json.url)).arrayBuffer());
    else throw new Error(`/api/say JSON without audio`);
  } else buf = Buffer.from(await res.arrayBuffer());
  if (!buf || buf.length < 256) throw new Error(`/api/say tiny body (${buf?.length})`);
  writeFileSync(outFile, buf);
}

// whisper.cpp -ml 1 tokens → words (the leading-space merge — yc-ref/lib/words.mjs)
// Punctuation-only tokens merge their TEXT but never their TIME: whisper
// stamps a trailing "." across the line's silent tail, and letting it extend
// the previous word's window is what collapsed round 3's "synthesizer" line
// (rescaleHeard saw a word "ending" at the file end and squashed the axis).
const punctOnly = (s) => /^[^\w'"“”‘’(]+$/.test(s);
function wordsFromWhisper(jsonPath) {
  const raw = JSON.parse(readFileSync(jsonPath, "utf8"));
  const words = [];
  for (const seg of raw.transcription) {
    const piece = seg.text;
    if (!piece || !piece.trim()) continue;
    const text = piece.trim();
    if (piece.startsWith(" ") || words.length === 0) {
      words.push({ text, fromMs: seg.offsets.from, toMs: seg.offsets.to });
    } else {
      const prev = words[words.length - 1];
      prev.text += text;
      if (!punctOnly(text)) prev.toMs = seg.offsets.to;
    }
  }
  const merged = [];
  for (const w of words) {
    if (punctOnly(w.text) && merged.length > 0) {
      const prev = merged[merged.length - 1];
      prev.text += w.text;
    } else merged.push(w);
  }
  return merged;
}

const norm = (s) => (s || "").toLowerCase().replace(/[^a-z']/g, "");

// Whisper writes spoken numbers as digits ("a hundred twenty eight" → "128"),
// which norms to nothing and derails alignment. Expand a digit word back into
// its spoken words, splitting the window char-proportionally.
function numberToWords(n) {
  const ones = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    "ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen",
    "eighteen", "nineteen"];
  const tens = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety"];
  if (n < 20) return [ones[n]];
  if (n < 100) return n % 10 ? [tens[Math.floor(n / 10)], ones[n % 10]] : [tens[Math.floor(n / 10)]];
  if (n < 1000) {
    const rest = n % 100 ? numberToWords(n % 100) : [];
    return [ones[Math.floor(n / 100)], "hundred", ...rest];
  }
  return [String(n)];
}
function expandDigitWords(words) {
  const out = [];
  for (const w of words) {
    const digits = w.text.replace(/[^\d]/g, "");
    if (!digits || !/^\W*\d[\d,]*\W*$/.test(w.text)) { out.push(w); continue; }
    const parts = numberToWords(parseInt(digits, 10));
    const chars = parts.reduce((s, p) => s + p.length, 0);
    let t0 = w.fromMs;
    const span = w.toMs - w.fromMs;
    for (const p of parts) {
      const dw = (span * p.length) / chars;
      out.push({ text: p, fromMs: Math.round(t0), toMs: Math.round(t0 + dw) });
      t0 += dw;
    }
  }
  return out;
}
function editDist(a, b) {
  const m = a.length, n = b.length;
  let prev = Array.from({ length: n + 1 }, (_, j) => j);
  for (let i = 1; i <= m; i++) {
    const curr = [i];
    for (let j = 1; j <= n; j++) {
      curr[j] = a[i - 1] === b[j - 1] ? prev[j - 1] : 1 + Math.min(prev[j - 1], prev[j], curr[j - 1]);
    }
    prev = curr;
  }
  return prev[n];
}

// ── whisper round-trip intelligibility (round 4 — jeffrey's hard gate) ─────
// "confirm with whisper end-to-end that we can still machine read the output
// sung lyrics." Rendered vocals are transcribed back and scored per line.
const STOPWORDS = new Set(["a", "an", "the", "in", "on", "of", "to", "for",
  "your", "its", "is", "and", "at", "up", "out"]);
const HOMOPHONES = {
  to: "two", too: "two", for: "four", fore: "four", ate: "eight",
  won: "one", banned: "band", cord: "chord", cords: "chords", suss: "sus",
  knew: "new", oar: "or", ore: "or", write: "right", rite: "right",
};
// casual contractions whisper expands ("wanna" → "want to") — canonicalize
// BOTH sides to the expansion so the outro CTA scores verbatim
const CONTRACTIONS = { wanna: ["want", "to"], gonna: ["going", "to"], gotta: ["got", "to"] };
function normTokens(text) {
  const rough = String(text).toLowerCase()
    .replace(/[’‘]/g, "'")
    .replace(/[.…]+/g, " ")          // dots split welded "menuband.app"
    .replace(/[^a-z0-9' ]+/g, " ")
    .split(/\s+/).filter(Boolean);
  const out = [];
  for (const w of rough) {
    if (/^\d+$/.test(w)) out.push(...numberToWords(parseInt(w, 10)));
    else if (CONTRACTIONS[w]) out.push(...CONTRACTIONS[w]);
    else out.push(w.replace(/'/g, ""));
  }
  return out.map((w) => HOMOPHONES[w] || w);
}

// v6 letter folding (the scales ladder): "C-D-E" and "see dee ee" must score
// as the same thing. Single tokens a–n become their letter names; welded
// letter runs ("cde") split first. Only applied on lettered lines.
function letterFoldTokens(toks) {
  const out = [];
  for (const t of toks) {
    if (/^[a-n]$/.test(t)) out.push(LETTER_WORDS[t]);
    else if (/^[a-n]{2,}$/.test(t) && !LETTER_NAME_SET.has(t) && !HOMOPHONES[t])
      out.push(...t.split("").map((c) => LETTER_WORDS[c]));
    else out.push(t);
  }
  return out;
}
const tokEq = (a, b) =>
  a === b || (a.length >= 3 && b.length >= 3 && editDist(a, b) <= 1);
// merge hyp tokens that SPLIT one ref token ("full screen" vs "fullscreen",
// whisper's hyphenation) — the mirror of dewedge below
function rewedge(refToks, hypToks) {
  const out = [];
  for (let i = 0; i < hypToks.length; i++) {
    let merged = false;
    for (let k = 4; k >= 2 && !merged; k--) {
      if (i + k > hypToks.length) continue;
      const cat = hypToks.slice(i, i + k).join("");
      if (refToks.some((r) => tokEq(r, cat))) {
        out.push(cat);
        i += k - 1;
        merged = true;
      }
    }
    if (!merged) out.push(hypToks[i]);
  }
  return out;
}
// split hyp tokens that are welds of consecutive ref tokens ("menubandapp")
function dewedge(refToks, hypToks) {
  const out = [];
  let i = 0;
  for (const h of hypToks) {
    let done = false;
    for (let s = i; s < refToks.length && !done; s++) {
      if (tokEq(refToks[s], h)) { out.push(refToks[s]); i = s + 1; done = true; break; }
      for (let k = 2; k <= 4 && s + k <= refToks.length; k++) {
        const cat = refToks.slice(s, s + k).join("");
        if (editDist(cat, h) <= Math.max(1, Math.ceil(cat.length / 5))) {
          out.push(...refToks.slice(s, s + k));
          i = s + k; done = true; break;
        }
      }
    }
    if (!done) out.push(h);
  }
  return out;
}
// token-level Levenshtein with fuzzy (≤1 edit) substitutions counted as hits
function werScore(refToks, hypToks) {
  const m = refToks.length, n = hypToks.length;
  let prev = Array.from({ length: n + 1 }, (_, j) => j);
  for (let i = 1; i <= m; i++) {
    const curr = [i];
    for (let j = 1; j <= n; j++) {
      const sub = (tokEq(refToks[i - 1], hypToks[j - 1]) ? 0 : 1) + prev[j - 1];
      curr[j] = Math.min(sub, 1 + prev[j], 1 + curr[j - 1]);
    }
    prev = curr;
  }
  return m ? prev[n] / m : 0;
}
function evalWER(refText, hypText, { lettered = false } = {}) {
  let ref = normTokens(refText);
  let hypRaw = normTokens(hypText);
  if (lettered) { ref = letterFoldTokens(ref); hypRaw = letterFoldTokens(hypRaw); }
  const hyp = dewedge(ref, rewedge(ref, hypRaw));
  const wer = +werScore(ref, hyp).toFixed(3);
  const missing = ref.filter((w) =>
    w.length >= 3 && !STOPWORDS.has(w) && !hyp.some((h) => tokEq(w, h)));
  return { wer, missing, pass: wer <= WER_GATE && missing.length === 0 };
}

// whisper-cli full transcription (segments, not -ml 1) of any audio file —
// always re-runs (renders change under the same path).
function whisperTranscribe(audioPath) {
  const wav16 = audioPath.replace(/\.(wav|mp3)$/, "") + "-tx16k.wav";
  sh("ffmpeg", ["-y", "-v", "error", "-i", audioPath, "-ac", "1", "-ar", "16000", wav16]);
  const oPath = wav16.replace(/\.wav$/, "");
  sh("whisper-cli", ["-m", WHISPER_MODEL, "-f", wav16, "-oj", "-of", oPath],
    { stdio: ["ignore", "ignore", "pipe"] });
  const j = JSON.parse(readFileSync(`${oPath}.json`, "utf8"));
  return j.transcription.map((s) => s.text).join(" ").replace(/\s+/g, " ").trim();
}
// transcribe a time span of a longer file (per-line stem checks)
function whisperTranscribeSpan(audioPath, t0, t1, outBase) {
  const seg = `${outBase}.wav`;
  sh("ffmpeg", ["-y", "-v", "error", "-i", audioPath,
    "-ss", Math.max(0, t0).toFixed(3), "-to", t1.toFixed(3),
    "-ac", "1", "-ar", "16000", seg]);
  sh("whisper-cli", ["-m", WHISPER_MODEL, "-f", seg, "-oj", "-of", outBase],
    { stdio: ["ignore", "ignore", "pipe"] });
  const j = JSON.parse(readFileSync(`${outBase}.json`, "utf8"));
  return j.transcription.map((s) => s.text).join(" ").replace(/\s+/g, " ").trim();
}

// Whisper loves to weld words together ("menu band dot app" → "menuband.app.",
// "a synthesizer" → "synthesizer") — align-words' midpoint split then hands a
// tiny function word HALF the audio. Pre-split any heard word whose norm is
// (fuzzily) the concatenation of the next 2-4 score words, char-proportionally,
// so every score word owns roughly its own audio before alignment runs.
function presplitHeard(scoreWords, heard) {
  const out = [];
  let i = 0;
  for (const h of heard) {
    const hn = norm(h.text);
    // best k = the join of score words that lands CLOSEST to this heard word —
    // and a merge only wins if it beats the single next score word outright
    // (otherwise "synthesizer" would swallow the "now" after it).
    const singleDist = i < scoreWords.length ? editDist(norm(scoreWords[i]), hn) : Infinity;
    let best = 0, bestDist = singleDist;
    for (let k = 2; k <= 4 && i + k <= scoreWords.length; k++) {
      const cat = scoreWords.slice(i, i + k).map(norm).join("");
      const d = editDist(cat, hn);
      // ties prefer the LONGER join — "menubanddot" and "menubanddotapp" are
      // equidistant from a heard "menubandapp.", and only the full join
      // leaves no score word stranded past the end of the audio.
      if (d <= Math.max(1, Math.ceil(cat.length / 4)) && d <= bestDist && d < singleDist) {
        best = k; bestDist = d;
      }
    }
    if (best >= 2) {
      const parts = scoreWords.slice(i, i + best).map(norm);
      const chars = parts.reduce((s, p) => s + Math.max(1, p.length), 0);
      let t0 = h.fromMs;
      const span = h.toMs - h.fromMs;
      for (const p of parts) {
        const w = (span * Math.max(1, p.length)) / chars;
        out.push({ text: p, fromMs: Math.round(t0), toMs: Math.round(t0 + w) });
        t0 += w;
      }
      i += best;
    } else {
      out.push(h);
      if (i < scoreWords.length) {
        const sn = norm(scoreWords[i]);
        if (sn === hn || editDist(sn, hn) <= 2 || (sn.length >= 3 && hn.startsWith(sn))) i++;
      }
    }
  }
  return out;
}

// v6 (scales): before alignment, fold whisper's heard letters onto the
// spelled letter-name score words — "C" → "see", and welded runs ("CDE",
// or dashes normed away: "c-d-e" → "cde") split char-proportionally into
// one window per letter. Only for lettered lines.
function letterizeHeard(heard) {
  const out = [];
  for (const h of heard) {
    const t = norm(h.text);
    if (/^[a-n]$/.test(t)) { out.push({ ...h, text: LETTER_WORDS[t] }); continue; }
    if (/^[a-n]{2,}$/.test(t) && !LETTER_NAME_SET.has(t)) {
      const span = h.toMs - h.fromMs;
      let t0 = h.fromMs;
      for (let c = 0; c < t.length; c++) {
        const w = span / t.length;
        out.push({ text: LETTER_WORDS[t[c]], fromMs: Math.round(t0), toMs: Math.round(t0 + w) });
        t0 += w;
      }
      continue;
    }
    out.push(h);
  }
  return out;
}

// Whisper stamps the words at a clip's hard end zero-width and buries their
// audio inside the PREVIOUS window ("Type" 110-1020 actually holds "type to
// play"). Clamp every window into the audio, then repair each RUN of
// degenerate windows by re-splitting the host span (the last good window
// through the run's end) at its quietest energy dips — the gaps between the
// words whisper welded together.
function energySplit(audio, aMs, bMs, n) {
  const hop = Math.floor(SR / 100);        // 10ms
  const a = Math.max(0, Math.floor((aMs / 1000) * SR));
  const b = Math.min(audio.length, Math.floor((bMs / 1000) * SR));
  let env = [];
  for (let s = a; s + hop <= b; s += hop) {
    let e = 0;
    for (let k = s; k < s + hop; k++) e += audio[k] * audio[k];
    env.push(Math.sqrt(e / hop));
  }
  // Trim the span to actual SPEECH: a whisper window welded to the file end
  // drags a silent tail in, and dips picked there would hand words silence.
  const thr = Math.max(0.008, 0.15 * Math.max(...env, 0));
  let f = env.findIndex((e) => e >= thr);
  let l = env.length - 1;
  while (l > 0 && env[l] < thr) l--;
  if (f > 0 || l < env.length - 1) {
    f = Math.max(0, f - 2); l = Math.min(env.length - 1, l + 4);
    aMs = aMs + f * 10; bMs = aMs + (l - f + 1) * 10;
    env = env.slice(f, l + 1);
  }
  const bounds = [Math.round(aMs)];
  if (n > 1) {
    const lo = Math.floor(env.length * 0.12), hi = Math.ceil(env.length * 0.94);
    const minSep = Math.max(3, Math.floor(env.length / (n * 2)));
    const cand = [];
    for (let k = lo; k < hi; k++) cand.push([env[k], k]);
    cand.sort((x, y) => x[0] - y[0]);
    const picks = [];
    for (const [, k] of cand) {
      if (picks.length >= n - 1) break;
      if (picks.every((p) => Math.abs(p - k) >= minSep)) picks.push(k);
    }
    while (picks.length < n - 1) picks.push(Math.floor(((picks.length + 1) / n) * env.length));
    picks.sort((x, y) => x - y);
    for (const p of picks) bounds.push(Math.round(aMs + p * 10));
  }
  bounds.push(Math.round(bMs));
  return bounds;
}

// On short exclamation lines whisper sometimes smears the whole timestamp
// axis past the real speech ("sings out" placed in post-speech silence). If
// even the PENULTIMATE word ends after the audible speech does, the axis is
// stretched — linearly rescale every window onto the real speech span.
function rescaleHeard(heard, audio, lineLenMs) {
  if (heard.length < 2) return heard;
  const hop = Math.floor(SR / 100);
  const env = [];
  for (let s = 0; s + hop <= audio.length; s += hop) {
    let e = 0;
    for (let k = s; k < s + hop; k++) e += audio[k] * audio[k];
    env.push(Math.sqrt(e / hop));
  }
  const thr = Math.max(0.008, 0.15 * Math.max(...env, 0));
  let f = env.findIndex((e) => e >= thr);
  let l = env.length - 1;
  while (l > 0 && env[l] < thr) l--;
  const speechStart = Math.max(0, f * 10 - 20);
  const speechEnd = Math.min(lineLenMs, (l + 1) * 10 + 40);
  if (heard[heard.length - 2].toMs <= speechEnd + 120) return heard;
  const wStart = heard[0].fromMs;
  const wEnd = Math.max(...heard.map((h) => h.toMs));
  const scale = (speechEnd - speechStart) / Math.max(1, wEnd - wStart);
  for (const h of heard) {
    h.fromMs = Math.round(speechStart + (h.fromMs - wStart) * scale);
    h.toMs = Math.round(speechStart + (h.toMs - wStart) * scale);
  }
  return heard;
}

function repairWindows(windows, lineLenMs, audio) {
  for (const w of windows) {
    w.toMs = Math.min(w.toMs, lineLenMs);
    w.fromMs = Math.min(w.fromMs, Math.max(0, w.toMs - 40));
  }
  let i = 0;
  while (i < windows.length) {
    if (windows[i].toMs - windows[i].fromMs >= 60) { i++; continue; }
    let j = i;
    while (j < windows.length && windows[j].toMs - windows[j].fromMs < 60) j++;
    const host = i > 0 ? windows[i - 1] : null;
    const hostStart = host ? host.fromMs : 0;
    // A degenerate run at the LINE END: whisper either stamped the words
    // zero-width at the file end with their audio still AHEAD (then the
    // remaining speech after the host's end is theirs — keep the host's own
    // good boundary! re-splitting from the host start is how "now" stole the
    // "si" out of round 4's "synthesizer") … or buried their audio INSIDE
    // the host window ("Type" 110-1020 holding "type to play") — then fall
    // back to re-splitting the host span.
    if (j >= windows.length && host) {
      const tail = energySplit(audio, host.toMs, lineLenMs, j - i);
      const speechMs = tail[tail.length - 1] - tail[0];
      if (speechMs >= 120 * (j - i)) {
        for (let w = i, k = 0; w < j; w++, k++) {
          windows[w].fromMs = tail[k]; windows[w].toMs = tail[k + 1];
        }
        i = j; continue;
      }
    }
    const hostEnd = j >= windows.length ? lineLenMs
      : Math.min(lineLenMs, Math.max(windows[j - 1].toMs, host ? host.toMs : 0));
    const parts = (host ? 1 : 0) + (j - i);
    const bounds = energySplit(audio, hostStart, hostEnd, parts);
    let k = 0;
    if (host) { host.fromMs = bounds[0]; host.toMs = bounds[1]; k = 1; }
    for (let w = i; w < j; w++, k++) { windows[w].fromMs = bounds[k]; windows[w].toMs = bounds[k + 1]; }
    i = j;
  }
  // Adjacent words must never share audio (round 3's "mac app" overlap sang
  // the same /æ/ on two beats) — clamp overlaps to the midpoint.
  for (let k = 1; k < windows.length; k++) {
    const a = windows[k - 1], b = windows[k];
    if (b.fromMs < a.toMs) {
      const mid = Math.round((Math.max(b.fromMs, a.fromMs) + a.toMs) / 2);
      a.toMs = Math.max(a.fromMs + 40, mid);
      b.fromMs = Math.min(a.toMs, Math.max(b.toMs - 40, mid));
    }
  }
  return windows;
}

// ── build absolute slots per word from the spec ────────────────────────────
function buildLines(slug) {
  const spec = LYRICS[slug];
  if (!spec) throw new Error(`no lyrics for ${slug}`);
  const score = JSON.parse(readFileSync(`${OUT}/${slug}.notes.json`, "utf8"));
  const lines = [];
  if (spec.mode === "lead") {
    const lead = (score.notes || []).filter((n) => n.lane === "lead").sort((a, b) => a.t - b.t);
    const need = spec.lines.reduce((s, l) => s + l.words.reduce((a, [, n]) => a + n, 0), 0);
    if (need !== lead.length) throw new Error(`${slug}: lyric syllables ${need} != lead notes ${lead.length}`);
    let k = 0;
    for (const line of spec.lines) {
      const words = line.words.map(([w, n]) => {
        const slots = lead.slice(k, k + n).map((s) => ({ t: s.t, dur: s.dur, midi: s.midi + spec.transpose }));
        k += n;
        return { w, slots };
      });
      lines.push({ tts: line.tts, words });
    }
  } else if (spec.mode === "scales") {
    // spoken frame + the sung letter ladder, all timed by the shared
    // score sidecar (render-jingles.mjs writes it; sim-scales reads it too)
    const sc = JSON.parse(readFileSync(`${OUT}/${slug}.score.json`, "utf8"));
    const asc = sc.ladder.filter((n) => n.dir === "up");
    const desc = sc.ladder.filter((n) => n.dir === "down");
    // notationText: the spelled letter names WITH commas — notation.mjs
    // matches its tokens to the lyric words, so every letter becomes its own
    // phrase and the engine ARTICULATES the ladder (tiny onset gaps, no
    // melisma blur) instead of welding 14 letters into one glissando.
    const sungLine = (tts, notes) => ({
      tts, lettered: true,
      notationText: notes.map((n) => LETTER_WORDS[n.letter]).join(", ") + ".",
      words: notes.map((n) => ({
        w: LETTER_WORDS[n.letter], display: n.letter,
        slots: [{ t: n.t, dur: n.dur, midi: n.vocal }],
      })),
    });
    lines.push({ tts: spec.intro, spoken: true, t: sc.spoken.intro.t, words: [] });
    // lettered: the run's welded "C-D-E-F-G-A-B" caption splits into letters
    lines.push({ tts: spec.run, spoken: true, lettered: true, t: sc.spoken.run.t, words: [] });
    lines.push(sungLine(spec.asc, asc));
    lines.push(sungLine(spec.desc, desc));
    lines.push({ tts: spec.outro, spoken: true, t: sc.spoken.outro.t, words: [] });
  } else {
    for (const line of spec.lines) {
      lines.push({
        tts: line.tts,
        words: line.words.map(([w, slots]) => ({ w, slots: slots.map(([t, dur, midi]) => ({ t, dur, midi })) })),
      });
    }
  }
  return { lines, durationSec: score.durationSec, spec };
}

// ── main ───────────────────────────────────────────────────────────────────
async function singOne(slug) {
  console.log(`\n▸ ${slug} — jeffrey sings (line-continuous WORLD)`);
  const { lines, durationSec, spec } = buildLines(slug);
  // v6: register ladder — the asked lift first, then the kermit/strain
  // fallbacks; a line stops falling as soon as it passes the WER gate.
  const specRegister = spec.register ?? REGISTER;
  const regLadder = [specRegister, ...REGISTER_FALLBACKS.filter((r) => r < specRegister)];
  const OCT_OPT = spec.mode !== "scales";   // the ladder places its own pitches
  console.log(`  register +${specRegister} (fallback ladder ${regLadder.join(" → ")})`);
  const dir = `${OUT}/sung/${slug}`;
  mkdirSync(`${dir}/words`, { recursive: true });
  if (!existsSync(WHISPER_MODEL)) throw new Error(`whisper model missing: ${WHISPER_MODEL}`);
  if (!existsSync(VENV_PY)) throw new Error(`pop venv missing: ${VENV_PY}`);

  const master = new Float32Array(Math.ceil(durationSec * SR));
  const sungWords = [];
  const report = [];
  const qaLines = [];
  const lineSpans = [];   // absolute spans for the per-line stem re-transcribe
  const consSpans = [];   // v5: absolute consonant spans → extra bed duck
  const spokenPlacements = [];  // v6: spoken lines placed after level match
  const regFallbacks = [];      // v6: per-line register fallbacks (reported)
  if (!existsSync(GOALPOSTS)) {
    throw new Error(`goalposts missing: ${GOALPOSTS} — build with spinging goalposts`);
  }

  // flatten words across lines for next-word lookahead
  const flat = [];
  lines.forEach((line, li) => line.words.forEach((w) => flat.push({ ...w, li })));

  for (let li = 0; li < lines.length; li++) {
    const line = lines[li];
    const hash = createHash("sha1").update(line.tts).digest("hex").slice(0, 8);
    const mp3 = `${dir}/line-${li}-${hash}.mp3`;
    await ttsLine(line.tts, mp3);

    // ── v6 spoken lines (the scales frame): natural TTS placed verbatim at
    // its absolute time — no engine, no pitch; whisper word timings become
    // the caption windows and the transcript must be essentially verbatim ──
    if (line.spoken) {
      const w16s = mp3.replace(/\.mp3$/, "-16k.wav");
      if (!existsSync(w16s)) {
        sh("ffmpeg", ["-y", "-v", "error", "-i", mp3, "-ac", "1", "-ar", "16000", w16s]);
      }
      const wjs = mp3.replace(/\.mp3$/, "-words");
      if (!existsSync(`${wjs}.json`)) {
        sh("whisper-cli", ["-m", WHISPER_MODEL, "-f", w16s, "-ml", "1", "-oj", "-ojf", "-of", wjs],
          { stdio: ["ignore", "ignore", "pipe"] });
      }
      const heard = wordsFromWhisper(`${wjs}.json`);
      const { audio: spokenAudio } = decodeAudioMono(mp3, SR);
      spokenPlacements.push({ audio: spokenAudio, at: Math.floor(line.t * SR) });
      // caption words: strip trailing commas/periods; a lettered spoken line
      // (the "C, D, E…" run) splits welded letter runs into single capitals
      let capWords = heard;
      if (line.lettered) {
        capWords = [];
        for (const h of heard) {
          const tN = norm(h.text);
          if (/^[a-n]{2,}$/.test(tN) && !LETTER_NAME_SET.has(tN)) {
            const span = (h.toMs - h.fromMs) / tN.length;
            let t0 = h.fromMs;
            for (const c of tN) {
              capWords.push({ text: c.toUpperCase(), fromMs: Math.round(t0), toMs: Math.round(t0 + span) });
              t0 += span;
            }
          } else if (/^[a-n]$/.test(tN)) capWords.push({ ...h, text: tN.toUpperCase() });
          else capWords.push(h);
        }
      }
      for (const h of capWords) {
        const text = h.text.replace(/[.,;:]+$/, "");
        if (!text) continue;
        sungWords.push({
          text, fromMs: Math.round(line.t * 1000 + h.fromMs),
          toMs: Math.round(line.t * 1000 + h.toMs), line: li, spoken: true,
        });
      }
      const tx = whisperTranscribe(mp3);
      const wr = evalWER(line.tts, tx, { lettered: !!line.lettered });
      console.log(`  line ${li} (spoken): "${line.tts}" → heard "${tx}" · WER ${wr.wer} ${wr.pass ? "✓" : "✗"}`);
      qaLines.push({
        line: li, text: line.tts, spoken: true,
        whisper: { transcript: tx, wer: wr.wer, missing: wr.missing, pass: wr.pass },
      });
      lineSpans.push({
        li, text: line.tts, lettered: !!line.lettered, spoken: true,
        t0: Math.max(0, line.t - 0.1),
        t1: Math.min(durationSec, line.t + spokenAudio.length / SR + 0.15),
      });
      continue;
    }

    // 16k mono for whisper (unpadded — trailing silence makes whisper smear
    // word timestamps into it; repairWindows handles the zero-width final
    // word it stamps at a hard file end), 48k mono for the WORLD engine
    const w16 = mp3.replace(/\.mp3$/, "-16k.wav");
    if (!existsSync(w16)) {
      sh("ffmpeg", ["-y", "-v", "error", "-i", mp3, "-ac", "1", "-ar", "16000", w16]);
    }
    const w48 = mp3.replace(/\.mp3$/, "-48k.wav");
    if (!existsSync(w48)) {
      sh("ffmpeg", ["-y", "-v", "error", "-i", mp3, "-ac", "1", "-ar", String(SR), w48]);
    }
    const wj = mp3.replace(/\.mp3$/, "-words");
    if (!existsSync(`${wj}.json`)) {
      sh("whisper-cli", ["-m", WHISPER_MODEL, "-f", w16, "-ml", "1", "-oj", "-ojf", "-of", wj],
        { stdio: ["ignore", "ignore", "pipe"] });
    }
    const { audio: lineAudio } = decodeAudioMono(mp3, SR);
    const lineLen = lineAudio.length / SR;
    const mapWords = line.words.map((w) => w.w);
    let heardRaw = expandDigitWords(wordsFromWhisper(`${wj}.json`));
    if (line.lettered) heardRaw = letterizeHeard(heardRaw);   // v6 scales
    const heard = presplitHeard(mapWords,
      rescaleHeard(heardRaw, lineAudio, lineLen * 1000));
    const windows = repairWindows(alignWords(mapWords, heard), lineLen * 1000, lineAudio);

    console.log(`  line ${li}: "${line.tts}" · whisper heard "${heard.map((h) => h.text).join(" ")}"`);

    // ── choral notation sidecar (round 4: phrase grouping lives HERE — lyric
    // punctuation + melody rests ≥ 0.4 s; the engine bridges legato inside a
    // phrase and breathes only at its edges). Phonemes from curated US IPA.
    const score = await buildLineScore({
      text: line.notationText ?? line.tts,   // v6: scales phrases per letter
      words: line.words.map((w) => ({ w: w.w, slots: w.slots })),
    });
    const scorePath = `${dir}/words/line-${li}-score.json`;
    writeLineScore(scorePath, score);
    const phraseStartOf = new Array(line.words.length).fill(false);
    for (const n of score.notes) {
      if (n.syllableIndex === 0 && n.articulation === "phraseStart") {
        phraseStartOf[n.wordIndex] = true;
      }
    }

    // ── plan the line for sing_line_world.py ───────────────────────────────
    const planWords = [];
    for (let wi = 0; wi < line.words.length; wi++) {
      const word = line.words[wi];
      const win = windows[wi];
      const slots = word.slots;
      const tStart = slots[0].t;
      const last = slots[slots.length - 1];
      const globalIdx = lines.slice(0, li).reduce((s, l) => s + l.words.length, 0) + wi;
      const next = flat[globalIdx + 1];
      let tEnd = last.t + Math.min(last.dur, 1.8);
      // stop before the next word — a bigger reserve across lines, where the
      // next line's onset cluster (which python can't see) needs room
      if (next) tEnd = Math.min(tEnd, next.slots[0].t - (next.li === li ? 0.01 : 0.12));
      if (tEnd <= tStart + 0.1) tEnd = tStart + 0.1;

      // padded source window — consonants live just outside whisper's window,
      // clamped to the neighbouring words' midpoints
      const prevWin = wi > 0 ? windows[wi - 1] : null;
      const nextWin = wi + 1 < windows.length ? windows[wi + 1] : null;
      let s0 = win.fromMs - 60;
      let s1 = win.toMs + 100;
      if (prevWin) s0 = Math.max(s0, (prevWin.toMs + win.fromMs) / 2);
      if (nextWin) s1 = Math.min(s1, (win.toMs + nextWin.fromMs) / 2 + 20);
      s0 = Math.max(0, s0); s1 = Math.min(lineLen * 1000, s1);

      planWords.push({
        w: word.w, wordIndex: wi, srcFromMs: Math.round(s0), srcToMs: Math.round(s1),
        slots, hardEnd: +tEnd.toFixed(4), phraseStart: phraseStartOf[wi],
      });
      sungWords.push({
        text: word.display ?? word.w,   // v6: the scales karaoke shows LETTERS
        fromMs: Math.round(tStart * 1000), toMs: Math.round(tEnd * 1000), line: li,
      });
    }

    // karaoke windows ride the legato bridges — a bridged word keeps
    // sounding until the next word starts, so its caption should too
    const lineWord0 = sungWords.length - line.words.length;
    for (let wi = 0; wi + 1 < line.words.length; wi++) {
      if (phraseStartOf[wi + 1]) continue;
      const cur = sungWords[lineWord0 + wi];
      const nxt = sungWords[lineWord0 + wi + 1];
      const gap = nxt.fromMs - cur.toMs;
      if (gap > 0 && gap <= BRIDGE_MAX_S * 1000) cur.toMs = nxt.fromMs;
    }

    const lineT0 = Math.max(0, planWords[0].slots[0].t - 0.35);
    const lineT1 = Math.min(durationSec, planWords[planWords.length - 1].hardEnd + 0.4);
    const outWav = `${dir}/words/line-${li}-sung.wav`;
    const planPath = `${dir}/words/line-${li}-plan.json`;

    // ── render + percentile-gate: iterate until the line sits inside the
    // reference bands (or the pass budget runs out) ────────────────────────
    // drift starts at the calibrated point where announce's lines land inside
    // the reference plateau-drift band on pass 1
    const tweaks = { drift_scale: 1.6, glide_scale: 1, vib_depth_scale: 1, beta_scale: 1, air_scale: 1,
      cons_stretch_scale: 1 };   // v5: consonant diction stretch (engine caps at 2.5×)
    const renderPlan = (register) => {
      const plan = {
        line_wav: w48,
        out_wav: outWav,
        lead_wav: `${dir}/words/line-${li}-lead.wav`,
        phoneme_sidecar: mp3.replace(/\.mp3$/, ".phonemes.json"),
        score: scorePath, goalposts: GOALPOSTS,
        line_t0: +lineT0.toFixed(4), line_t1: +lineT1.toFixed(4),
        harmony: HARMONY, seed: 7 + li,
        f0_floor: 60, f0_ceil: 300,        // jeffrey's real range — de-kermit
        // (the octave optimizer weighs words by voiced evidence instead of
        // narrowing this ceiling — the TTS exclamations genuinely peak high)
        octave_opt: OCT_OPT, choir: true,
        register,                          // v6: applied AFTER the octave fit
        tweaks,
        words: planWords,
      };
      writeFileSync(planPath, JSON.stringify(plan, null, 1));
      const wr = sh(VENV_PY, [WORLD_HELPER, planPath]);
      let st = {};
      try { st = JSON.parse(wr.stdout.trim().split("\n").pop()); } catch {}
      return st;
    };
    // ── render loop + whisper round-trip gate. v5: EVERY render (QA passes
    // and clarity passes alike) is transcribed and the best take wins —
    // conformance first (the goalposts arbitrate, musicality before WER),
    // then WER. Deterministic: no take lottery between equivalent passes.
    const leadWav = `${dir}/words/line-${li}-lead.wav`;
    const bestWav = `${dir}/words/line-${li}-sung-best.wav`;
    const bestLead = `${dir}/words/line-${li}-lead-best.wav`;
    const confOf = (s) => s?.conformance?._pass !== false;
    const best = { res: null, transcript: "", stats: null, conf: false };
    let lastIsBest = false;
    const consider = (st) => {
      const tx = whisperTranscribe(outWav);
      const wr = evalWER(line.tts, tx, { lettered: !!line.lettered });
      const cf = confOf(st);
      const wins = best.stats === null
        || (wr.wer < best.res.wer && (cf || !best.conf))
        || (wr.wer <= best.res.wer && cf && !best.conf);
      if (wins) {
        Object.assign(best, { res: wr, transcript: tx, stats: st, conf: cf });
        copyFileSync(outWav, bestWav);
        copyFileSync(leadWav, bestLead);
      }
      lastIsBest = wins;
      return wr;
    };
    let stats = {};
    let passes = 0;
    let clarityPasses = 0;
    const registersTried = [];
    // ── v6: register ladder — full QA + clarity budget per register; a line
    // stops falling as soon as the whisper gate passes. consider() keeps the
    // best take across ALL registers (ties prefer the earlier = higher one).
    for (const reg of regLadder) {
      registersTried.push(reg);
      Object.assign(tweaks, { drift_scale: 1.6, glide_scale: 1, vib_depth_scale: 1,
        beta_scale: 1, air_scale: 1, cons_stretch_scale: 1 });
      for (let pass = 1; pass <= QA_PASSES; pass++) {
        passes++;
        stats = renderPlan(reg);
        if (stats.error) break;
        const clean = stats.clicks && stats.clicks.clicks === 0 && stats.clicks.flux_spikes === 0;
        if ((!stats.conformance || stats.conformance._pass) && clean) { consider(stats); break; }
        consider(stats);
        if (pass === QA_PASSES) break;
        adjustTweaks(tweaks, stats.conformance);
        console.log(`    ↻ pass ${pass} (reg +${reg}): out of band — retweak ` +
          Object.entries(tweaks).map(([k, v]) => `${k}=${v.toFixed(2)}`).join(" "));
      }
      if (stats.error) break;
      for (let cp = 1; cp <= CLARITY_PASSES && !best.res.pass; cp++) {
        clarityPasses++;
        tweaks.air_scale *= 0.6;
        tweaks.vib_depth_scale *= 0.7;
        // v5: more diction, not just less air — lean the stretch in harder
        tweaks.cons_stretch_scale = Math.min(1.3, tweaks.cons_stretch_scale * 1.15);
        console.log(`    ↻ clarity pass ${cp} (reg +${reg}): WER ${best.res.wer} ` +
          `(heard "${best.transcript}") — re-render with less air/vibrato, more stretch`);
        const st = renderPlan(reg);
        if (st.error) break;
        consider(st);
      }
      if (best.res?.pass) break;
      if (reg !== regLadder[regLadder.length - 1]) {
        console.log(`    ↧ register +${reg} missed the WER gate (${best.res?.wer}) — falling back`);
      }
    }
    if (stats.error) {
      report.push({ slug, line: li, word: "(line)", note: stats.error });
      continue;
    }
    if (!lastIsBest) {
      // the final re-render didn't win — restore the best-scoring take
      copyFileSync(bestWav, outWav);
      copyFileSync(bestLead, leadWav);
    }
    let werRes = best.res;
    let transcript = best.transcript;
    stats = best.stats;
    // v6: which register actually won — a drop below the asked one is a
    // reported fallback (kermit/strain safety valve)
    const finalReg = best.stats?.register ?? regLadder[0];
    if (finalReg !== specRegister) {
      regFallbacks.push({ slug, line: li, text: line.tts,
        asked: specRegister, used: finalReg, wer: best.res.wer });
      console.log(`    ⤵ line ${li} register fallback: +${specRegister} → +${finalReg}`);
    }
    // v5 diagnostic: transcribe the choir-less LEAD stem too — separates
    // diction gains from choir masking in the QA sidecar
    const leadTranscript = whisperTranscribe(leadWav);
    const leadWer = evalWER(line.tts, leadTranscript, { lettered: !!line.lettered });
    // v5: absolute consonant spans drive the bed's extra diction duck
    for (const [a, b] of stats.consonant_spans || []) {
      consSpans.push([lineT0 + a, lineT0 + b]);
    }
    for (const w of stats.words || []) {
      report.push({
        slug, line: li, word: w.word, target: w.targets.join(","),
        detected: w.detected_midi ?? "—", shift: w.shift_st ?? "—",
        onsetMs: w.onset_ms, codaMs: w.coda_ms,
        note: w.sung ? undefined : "no voiced nuclei — sung unpitched",
      });
    }
    const confBits = stats.conformance
      ? Object.entries(stats.conformance)
        .filter(([k, v]) => !k.startsWith("_") && v && v.pass === false)
        .map(([k, v]) => `${k}=${v.value}∉[${v.lo},${v.hi}]`)
      : [];
    const cont = stats.voicing_continuity || {};
    const contPass = cont.min == null || cont.min >= CONTINUITY_GATE;
    report.push({
      slug, line: li, word: "(qa)", info:
        `f0 jumps max ${stats.f0_jump_max_cents}¢ p95 ${stats.f0_jump_p95_cents}¢ · ` +
        `oct ${stats.line_transpose >= 0 ? "+" : ""}${stats.line_transpose} · β ${stats.beta} · ` +
        `passes ${passes}+${clarityPasses} · conf ${stats.conformance ? (stats.conformance._pass ? "PASS" : "miss: " + confBits.join(" ")) : "n/a"} · ` +
        `clicks ${stats.clicks?.clicks ?? "?"}/${stats.clicks?.flux_spikes ?? "?"} · ` +
        `legato ${cont.min ?? "?"} (${cont.bridges ?? 0} bridges) ${contPass ? "PASS" : "FAIL"} · ` +
        `onset-jump ${stats.voiced_onset_jump_max_cents ?? "?"}¢ · ` +
        `WER ${werRes.wer} ${werRes.pass ? "PASS" : `FAIL missing:[${werRes.missing}]`} · heard "${transcript}" · ` +
        `lead-WER ${leadWer.wer} heard "${leadTranscript}"`,
    });
    if (!contPass) console.log(`  ⚠ line ${li}: voicing continuity ${cont.min} < ${CONTINUITY_GATE}`);
    if (!werRes.pass) console.log(`  ⚠ line ${li}: WER gate FAILED at ${werRes.wer} — heard "${transcript}"`);
    console.log(`    lead-only diagnostic: WER ${leadWer.wer} — heard "${leadTranscript}"`);
    qaLines.push({
      line: li, text: line.tts, passes, clarityPasses, tweaks,
      register: finalReg, registersTried,
      registerFallback: finalReg !== specRegister,
      lineTranspose: stats.line_transpose, beta: stats.beta, harmony: HARMONY,
      consStretchScale: stats.cons_stretch_scale,
      f0JumpMaxCents: stats.f0_jump_max_cents, f0JumpP95Cents: stats.f0_jump_p95_cents,
      voicingContinuity: cont, voicingContinuityPass: contPass,
      voicedOnsetJumpMaxCents: stats.voiced_onset_jump_max_cents,
      whisper: { transcript, wer: werRes.wer, missing: werRes.missing, pass: werRes.pass },
      whisperLead: { transcript: leadTranscript, wer: leadWer.wer,
        missing: leadWer.missing, pass: leadWer.pass },
      conformance: stats.conformance, clicks: stats.clicks,
    });
    lineSpans.push({
      li, text: line.tts, lettered: !!line.lettered,
      t0: Math.max(0, planWords[0].slots[0].t - 0.15),
      t1: Math.min(durationSec, planWords[planWords.length - 1].hardEnd + 0.3),
    });

    // place the whole line at its absolute time — one continuous render,
    // no per-word fades to smear
    const { audio: sung } = decodeAudioMono(outWav, SR);
    const at = Math.floor(lineT0 * SR);
    for (let i = 0; i < sung.length && at + i < master.length; i++) master[at + i] += sung[i];
  }

  // v6: place the spoken lines now, level-matched to the sung material —
  // active-frame RMS of the assembled sung vocal vs each spoken take, spoken
  // sitting just under the singing so the frame never shouts over the song
  if (spokenPlacements.length) {
    const rmsActive = (buf) => {
      let e = 0, n = 0;
      for (let i = 0; i < buf.length; i++) {
        const v = buf[i];
        if (Math.abs(v) > 1e-3) { e += v * v; n++; }
      }
      return n ? Math.sqrt(e / n) : 0;
    };
    const sungRms = rmsActive(master);
    for (const p of spokenPlacements) {
      const own = rmsActive(p.audio);
      const g = sungRms > 0 && own > 0 ? Math.min(1.6, (sungRms * 0.9) / own) : 1;
      for (let i = 0; i < p.audio.length && p.at + i < master.length; i++) {
        master[p.at + i] += p.audio[i] * g;
      }
    }
  }

  // normalize the vocal, write it, then MASTER the mix
  let peak = 0;
  for (let i = 0; i < master.length; i++) peak = Math.max(peak, Math.abs(master[i]));
  if (peak > 0) for (let i = 0; i < master.length; i++) master[i] *= 0.85 / peak;
  const vocalWav = `${OUT}/${slug}-vocal.wav`;
  writeWavF32(vocalWav, master);

  // soft reverb halo on the vocal bus (angelic) — quiet wet, short decay
  const vocalWet = `${OUT}/${slug}-vocal-wet.wav`;
  sh(VENV_PY, [VOCAL_BUS, "reverb", vocalWav, vocalWet, "-16", "1.1"]);

  const bed = `${OUT}/${slug}.mp3`;
  const mix = `${OUT}/${slug}-sung.mp3`;
  const bedDur = parseFloat(sh("ffprobe", ["-v", "error", "-show_entries", "format=duration",
    "-of", "csv=p=0", bed]).stdout.trim());

  // v5 mix diction aid: a control track marking the engine's consonant spans
  // drives an EXTRA bed duck — the level-following sidechain alone ducks
  // least exactly when the lead is quietest, i.e. on its consonants.
  const duckWav = `${OUT}/${slug}-consduck.wav`;
  {
    const ctrl = new Float32Array(master.length);
    const ramp = Math.floor(0.006 * SR);
    for (const [a, b] of consSpans) {
      const s0 = Math.max(0, Math.floor(a * SR));
      const s1 = Math.min(ctrl.length, Math.ceil(b * SR));
      for (let i = s0; i < s1; i++) {
        let g = 1;
        if (i - s0 < ramp) g = (i - s0) / ramp;
        if (s1 - i < ramp) g = Math.min(g, (s1 - i) / ramp);
        ctrl[i] = Math.max(ctrl[i], 0.35 * g);
      }
    }
    // 400 Hz carrier so the compressor's detector sees real level
    for (let i = 0; i < ctrl.length; i++) {
      ctrl[i] *= Math.sin((2 * Math.PI * 400 * i) / SR);
    }
    writeWavF32(duckWav, ctrl);
  }

  // Vocal bus: highpass → gentle 3:1 compression (slow-ish release) →
  // de-harsh (eased to 0.15 in v5 — the stretched sibilants ARE the diction),
  // then the bed ducks under it: first the consonant-span duck (~5 dB), then
  // the level sidechain. apad+atrim pin the mix to the bed's exact length —
  // the sims mux with -shortest, and a mix even a frame short would truncate
  // the video.
  const premaster = `${OUT}/${slug}-sung-premaster.wav`;
  sh("ffmpeg", ["-y", "-v", "error", "-i", bed, "-i", vocalWet, "-i", duckWav, "-filter_complex",
    "[1:a]aformat=sample_rates=48000:channel_layouts=stereo,highpass=f=70," +
    "acompressor=threshold=0.125:ratio=3:attack=12:release=250:makeup=2," +
    "deesser=i=0.15,asplit=2[sc][v];" +
    "[2:a]aformat=sample_rates=48000:channel_layouts=stereo[cd];" +
    "[0:a]aformat=sample_rates=48000:channel_layouts=stereo[b0];" +
    "[b0][cd]sidechaincompress=threshold=0.08:ratio=2:attack=5:release=90[b];" +
    "[b][sc]sidechaincompress=threshold=0.05:ratio=5:attack=12:release=220[duck];" +
    "[duck][v]amix=inputs=2:duration=first:normalize=0:weights=1 1.25[m];" +
    "[m]highpass=f=30,alimiter=limit=0.89:level=false,apad=pad_dur=2," +
    `atrim=0:${bedDur.toFixed(4)},asetpts=PTS-STARTPTS[out]`,
    "-map", "[out]", premaster]);

  // hard glitch gate on the premaster (waveform + spectral-flux click scan)
  const mixScan = JSON.parse(
    sh(VENV_PY, [VOCAL_BUS, "scan", premaster]).stdout.trim().split("\n").pop());
  if (mixScan.clicks > 0) {
    console.log(`  ⚠ click scan flagged ${mixScan.clicks} clicks at ${mixScan.positions_s}`);
  }

  // MASTER: two-pass loudnorm to -14 LUFS integrated / -1 dBTP
  const mjson = measureLoudnorm(premaster);
  sh("ffmpeg", ["-y", "-v", "error", "-i", premaster, "-af",
    `loudnorm=I=-14:TP=-1.5:LRA=11:measured_I=${mjson.input_i}:measured_TP=${mjson.input_tp}` +
    `:measured_LRA=${mjson.input_lra}:measured_thresh=${mjson.input_thresh}` +
    `:offset=${mjson.target_offset}:linear=true`,
    "-ar", "48000", "-c:a", "libmp3lame", "-q:a", "2", mix]);
  const verify = measureLoudnorm(mix);   // what actually landed on the mp3
  console.log(`  mastered: ${mjson.input_i} LUFS / ${mjson.input_tp} dBTP → ` +
    `${verify.input_i} LUFS / ${verify.input_tp} dBTP on disk`);

  // ── whisper end-to-end (round 4): per-line WER on the assembled vocal
  // STEM (pre-mix), then one smoke transcript of the final mixed reel ──────
  console.log(`  whisper end-to-end on the vocal stem …`);
  const stemLines = lineSpans.map((s) => {
    const tx = whisperTranscribeSpan(vocalWav, s.t0, s.t1, `${dir}/words/stem-line-${s.li}`);
    const w = evalWER(s.text, tx, { lettered: !!s.lettered });
    const mark = w.pass ? "✓" : "✗";
    console.log(`    ${mark} L${s.li} WER ${w.wer} "${s.text}" → heard "${tx}"`);
    return { line: s.li, text: s.text, transcript: tx, wer: w.wer, missing: w.missing, pass: w.pass };
  });
  const mixTranscript = whisperTranscribe(mix);
  console.log(`  mix smoke transcript: "${mixTranscript}"`);

  writeFileSync(`${OUT}/${slug}.words.sung.json`, JSON.stringify(sungWords, null, 2));
  writeFileSync(`${OUT}/${slug}-sung-qa.json`, JSON.stringify({
    slug, harmony: HARMONY, engine: "spinging/lib/sing_line_world.py (round 6)",
    goalposts: GOALPOSTS,
    register: { asked: specRegister, ladder: regLadder, fallbacks: regFallbacks },
    gates: { werMax: WER_GATE, voicingContinuityMin: CONTINUITY_GATE },
    consDuckSpans: consSpans.length,
    pronunciationSources: { ...sourceCounts },
    lines: qaLines,
    stemWhisper: stemLines,
    mixWhisperSmoke: mixTranscript,
    mixClickScan: mixScan,
    mastered: { lufs: parseFloat(verify.input_i), truePeakDb: parseFloat(verify.input_tp) },
  }, null, 1));
  console.log(`✓ ${mix}`);
  console.log(`✓ ${OUT}/${slug}.words.sung.json · ${sungWords.length} words`);
  console.log(`✓ ${OUT}/${slug}-sung-qa.json`);
  for (const f of regFallbacks) {
    report.push({ slug, line: f.line, word: "(register)",
      info: `register fallback +${f.asked} → +${f.used} (best WER ${f.wer}) — "${f.text}"` });
  }
  if (!regFallbacks.length && specRegister !== 0) {
    console.log(`  register: every line held +${specRegister}`);
  }
  return report;
}

const positional = process.argv.slice(2).filter((a, i, all) =>
  !a.startsWith("--") && all[i - 1] !== "--harmony");
const arg = positional[0] || "all";
const slugs = arg === "all" ? Object.keys(LYRICS) : [arg];
console.log(`harmony lock ${HARMONY} (β = ${(1 - HARMONY).toFixed(3)})`);
const allReports = [];
for (const slug of slugs) allReports.push(...(await singOne(slug)));
console.log(`\npronunciations: ${JSON.stringify(sourceCounts)}`);
console.log("\nword map (target ← detected, shift in st, onset/coda pickups in ms):");
for (const r of allReports) {
  if (r.info) { console.log(`  [${r.slug} L${r.line}] ${r.info}`); continue; }
  if (r.note) console.log(`  ⚠ [${r.slug} L${r.line}] "${r.word}" — ${r.note}`);
  else console.log(`  [${r.slug} L${r.line}] "${r.word}" → ${r.target} (det ${r.detected}, shift ${r.shift}, on ${r.onsetMs}ms, coda ${r.codaMs}ms)`);
}
