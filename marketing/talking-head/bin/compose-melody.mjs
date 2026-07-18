// compose-melody.mjs — turn a spoken transcript into a singable .np score.
//
// The YC monologue is 169 words. Hand-picking 169 notes would be a week of
// composition; letting the pitch follow the speech would be no melody at all.
// The middle path: a designed line that knows about SENTENCES. Each sentence
// gets a melodic arch — lift off the tonic, rise to a peak somewhere past the
// middle, settle back down and cadence on the tonic at the period. That's the
// shape of a sung phrase, and pinning it to the text's own sentences means the
// melody breathes where the speaker breathed.
//
//   node compose-melody.mjs <alignment.json> --out yc.np --words words.json
//
// Reads the ElevenLabs alignment (its `.words` are the sung word list + timing),
// writes the .np score and the words.json score-pitch needs.
//
// Key: D major pentatonic in a baritone octave — D3 E3 F#3 A3 B3 D4 E4 F#4 —
// which is where jeffrey's voice sits and where WORLD's shift reads as singing,
// not as artifacted speech (per pop/dance/bin/sing.mjs).

import { readFileSync, writeFileSync } from "node:fs";

const argv = process.argv.slice(2);
const flag = (n, d = null) => {
  const i = argv.indexOf(`--${n}`);
  return i >= 0 && argv[i + 1] ? argv[i + 1] : d;
};
const alignPath = argv.find((a) => !a.startsWith("--"));
const outNp = flag("out", "yc.np");
const outWords = flag("words", "words.json");

const align = JSON.parse(readFileSync(alignPath, "utf8"));
const words = align.words; // [{ text, fromMs, toMs }]

// The scale, low to high, as note names. Index into this and you're always in
// key. Two octaves of D-major pentatonic covers a comfortable sung range.
const SCALE = ["D3", "E3", "F#3", "A3", "B3", "D4", "E4", "F#4", "A4"];
const TONIC = 0; // index of D3
const FIFTH = 3; // index of A3 — where phrases lift off from

// A gentle arch across `n` words: start near the fifth, climb to a peak past the
// middle, fall to the tonic on the last word. Returns `n` scale indices.
function arch(n) {
  if (n === 1) return [TONIC];
  const peak = Math.min(SCALE.length - 1, FIFTH + 3); // F#4 region
  const peakAt = Math.floor(n * 0.62); // crest a bit past middle
  const out = [];
  for (let i = 0; i < n; i += 1) {
    let idx;
    if (i <= peakAt) {
      idx = FIFTH + Math.round(((peak - FIFTH) * i) / Math.max(1, peakAt));
    } else {
      const k = (i - peakAt) / Math.max(1, n - 1 - peakAt);
      idx = peak - Math.round((peak - TONIC) * k); // descend to tonic
    }
    out.push(Math.max(0, Math.min(SCALE.length - 1, idx)));
  }
  out[out.length - 1] = TONIC; // always cadence home
  return out;
}

// Break the word list into sentences on terminal punctuation, so each arch maps
// to a real spoken phrase.
const sentences = [];
let cur = [];
for (const w of words) {
  cur.push(w);
  if (/[.!?]$/.test(w.text.trim())) {
    sentences.push(cur);
    cur = [];
  }
}
if (cur.length) sentences.push(cur);

// A short sentence sung as a full octave arch sounds jumpy; a long one sung flat
// sounds dull. Softening the arch on very short sentences keeps it singable.
const lines = [];
const npWords = [];
for (const s of sentences) {
  const contour = arch(s.length);
  const notes = s.map((w, i) => {
    // strip punctuation for the syllable text the aligner matches on
    const syl = w.text.trim().replace(/[^A-Za-z']/g, "") || "la";
    npWords.push(w.text);
    // a beat per word; the last word of a sentence rings longer (a breath)
    const beats = i === s.length - 1 ? 3 : 1;
    return `${SCALE[contour[i]]}:${syl}*${beats}`;
  });
  lines.push(notes.join(" "));
}

const np = [
  "# yc-2018 — the YC application, sung.",
  "# One melodic arch per sentence: lift off the fifth, crest past the middle,",
  "# cadence home on the tonic. D-major pentatonic, baritone octave.",
  "# notation: NOTE:syllable*beats",
  "",
  "verse",
  ...lines,
  "",
].join("\n");

writeFileSync(outNp, np);
writeFileSync(outWords, JSON.stringify(words, null, 2));
console.log(`✓ ${outNp} — ${sentences.length} sentences, ${words.length} words`);
console.log(`✓ ${outWords}`);
