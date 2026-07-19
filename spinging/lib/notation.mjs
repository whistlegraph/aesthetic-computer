// notation.mjs — the spinging choral score (round 4).
//
// Choir-sheet syllable underlay married to jingle melodies: per NOTE —
//   { t, dur, midi, word, wordIndex, syllableIndex, syllable,
//     phonemes: { onset:[{ipa,cls,voiced}…], nucleus:{ipa,…}, coda:[…] },
//     melisma: "start"|"mid"|"end"|null,   // slots sharing one sung vowel
//     phrase: 0-based phrase index,
//     articulation: "phraseStart"|"phraseEnd"|"legato" }
//
// Built from the lyric spec (words + note slots) + definitive pronunciations
// (spinging/lib/pronounce.mjs — Wiktionary US first). Both the guided aligner
// and the WORLD synthesizer consume this sidecar; nobody guesses phonemes
// from spectra alone anymore.
//
// Round 4: PHRASE GROUPING is owned here — a phrase breaks at lyric
// punctuation (. ! ? , ; : —) and at melody rests ≥ REST_PHRASE_S. Within a
// phrase the engine bridges inter-word gaps legato (sustained vowel, gliding
// f0); breaths happen ONLY at phrase boundaries.
//
// API:
//   await buildLineScore({ text, words })   // words: [{ w, slots:[{t,dur,midi}] }]
//     → { version, text, notes:[…], phrases: n }
//   readLineScore(path) / writeLineScore(path, score)

import { readFileSync, writeFileSync } from "node:fs";
import { pronounce } from "./pronounce.mjs";

const VERSION = 4;
export const REST_PHRASE_S = 0.4; // melody rest ≥ this → phrase break (breathe)

// Fit a word's pronounced syllables onto its note slots.
//  - equal: 1:1
//  - more syllables than slots: merge the weakest (unstressed/schwa) syllable
//    into its neighbour — onset joins previous coda, coda joins next onset.
//  - fewer syllables than slots: melisma — the last syllable's vowel is held
//    across the extra notes (onset sung on the first, coda on the last).
function fitSyllables(sylls, nSlots) {
  let s = sylls.map((x) => ({
    onset: [...x.onset], nucleus: x.nucleus, coda: [...x.coda], stress: x.stress,
  }));
  while (s.length > nSlots && s.length > 1) {
    // weakest = unstressed schwa-ish, shortest phone count, never the first
    let k = -1, best = Infinity;
    for (let i = 1; i < s.length; i++) {
      const weight = (s[i].stress ? 10 : 0) + (/(ə|ɐ|ɘ)/.test(s[i].nucleus.ipa) ? 0 : 4) +
        s[i].onset.length + s[i].coda.length;
      if (weight < best) { best = weight; k = i; }
    }
    const dead = s[k];
    s[k - 1].coda.push(...dead.onset);
    if (k + 1 < s.length) s[k + 1].onset.unshift(...dead.coda);
    else s[k - 1].coda.push(...dead.coda);
    s.splice(k, 1);
  }
  // melisma expansion
  const out = [];
  for (let i = 0; i < s.length; i++) {
    const extra = i === s.length - 1 ? nSlots - s.length : 0;
    if (extra <= 0) { out.push({ ...s[i], melisma: null }); continue; }
    for (let m = 0; m <= extra; m++) {
      out.push({
        onset: m === 0 ? s[i].onset : [],
        nucleus: s[i].nucleus,
        coda: m === extra ? s[i].coda : [],
        stress: s[i].stress,
        melisma: m === 0 ? "start" : m === extra ? "end" : "mid",
      });
    }
  }
  return out.slice(0, nSlots);
}

const syllableText = (s) =>
  [...s.onset, s.nucleus, ...s.coda].map((p) => p.ipa).join("");

// Phrase grouping (round 4): word i STARTS a new phrase when
//   - it is the first word of the line, or
//   - the lyric text puts phrase punctuation right after word i-1, or
//   - the melody rests ≥ REST_PHRASE_S between word i-1's last note end
//     and word i's first note.
// Lyric words are matched to text tokens in order (the specs are written 1:1).
function phraseStarts(text, words) {
  const norm = (s) => (s || "").toLowerCase().replace(/[^a-z0-9']/g, "");
  const tokens = (text || "").split(/\s+/).filter(Boolean)
    .map((tok) => ({ word: norm(tok), breakAfter: /[.!?,;:—…]["')\]]*$/.test(tok) }));
  const starts = new Array(words.length).fill(false);
  starts[0] = true;
  let ti = 0;
  for (let wi = 0; wi < words.length; wi++) {
    const w = norm(words[wi].w);
    while (ti < tokens.length && tokens[ti].word !== w) ti++;
    const tok = ti < tokens.length ? tokens[ti] : null;
    ti++;
    if (tok?.breakAfter && wi + 1 < words.length) starts[wi + 1] = true;
    if (wi + 1 < words.length) {
      const prevSlots = words[wi].slots;
      const last = prevSlots[prevSlots.length - 1];
      const rest = words[wi + 1].slots[0].t - (last.t + last.dur);
      if (rest >= REST_PHRASE_S) starts[wi + 1] = true;
    }
  }
  return starts;
}

export async function buildLineScore({ text, words }) {
  const starts = phraseStarts(text, words);
  const notes = [];
  let phrase = -1;
  for (let wi = 0; wi < words.length; wi++) {
    const word = words[wi];
    if (starts[wi]) phrase++;
    const phraseEndsHere = wi === words.length - 1 || starts[wi + 1];
    const pron = await pronounce(word.w);
    const fitted = fitSyllables(pron.syllables, word.slots.length);
    for (let si = 0; si < word.slots.length; si++) {
      const slot = word.slots[si];
      const syl = fitted[si];
      const lastOfWord = si === word.slots.length - 1;
      notes.push({
        t: slot.t, dur: slot.dur, midi: slot.midi,
        word: word.w, wordIndex: wi, syllableIndex: si,
        syllable: syllableText(syl),
        ipa: pron.ipa, ipaSource: pron.source,
        phonemes: { onset: syl.onset, nucleus: syl.nucleus, coda: syl.coda },
        stress: syl.stress, melisma: syl.melisma,
        phrase,
        articulation: si === 0 && starts[wi] ? "phraseStart"
          : lastOfWord && phraseEndsHere ? "phraseEnd" : "legato",
      });
    }
  }
  return { version: VERSION, text, notes, phrases: phrase + 1 };
}

export function writeLineScore(path, score) {
  writeFileSync(path, JSON.stringify(score, null, 1));
}
export function readLineScore(path) {
  return JSON.parse(readFileSync(path, "utf8"));
}
