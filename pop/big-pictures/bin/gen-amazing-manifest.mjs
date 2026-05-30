#!/usr/bin/env node
// gen-amazing-manifest.mjs — emit an rfa.mjs-compatible manifest for
// amazing-grace verse 1, parsed from pop/big-pictures/amazing.np.
//
// rfa.mjs (pop/bin/rfa.mjs) reads pop/<lane>/voice-takes/manifest.json
// to know which notes/syllables to walk through. The big-pictures
// pipeline doesn't write one (cli.mjs runs ElevenLabs end-to-end), so
// we synthesize it here from amazing.np for the per-syllable recording
// flow described in pop/REQUEST-FOR-AUDIO.md.
//
// Output: pop/big-pictures/voice-takes/manifest.json
//   { track, lane, title, bpm, beatSec, barSec, scale, range, lyric,
//     notes: [{ id, bar, beat, midi, note, word, durBeats, durSec,
//                startSec, vowel, hasTake }] }
//
// Usage:  node pop/big-pictures/bin/gen-amazing-manifest.mjs

import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { resolve, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP  = resolve(HERE, "../..");
const SCORE = resolve(POP, "big-pictures/amazing.np");
const TAKES = resolve(POP, "big-pictures/voice-takes");
const OUT   = resolve(TAKES, "manifest.json");

const BPM = 70;
const BEAT_SEC = 60 / BPM;             // 0.857
const BAR_SEC  = 3 * BEAT_SEC;          // 3/4 waltz

// Note name → MIDI (G major-pentatonic range covers D3..D4 in verse 1).
function noteToMidi(s) {
    const m = s.match(/^([A-G])(#|b)?(-?\d+)$/);
    if (!m) throw new Error(`bad note: ${s}`);
    const pc = { C:0, D:2, E:4, F:5, G:7, A:9, B:11 }[m[1]];
    const acc = m[2] === "#" ? 1 : m[2] === "b" ? -1 : 0;
    const oct = parseInt(m[3], 10);
    return 12 * (oct + 1) + pc + acc;
}

const src = readFileSync(SCORE, "utf8");
const tokens = [];
for (const raw of src.split("\n")) {
    const line = raw.split("#")[0].trim();
    if (!line || /^verse\b/i.test(line)) continue;
    // Each token is NOTE:syllable*beats
    for (const tok of line.split(/\s+/)) {
        const m = tok.match(/^([A-G][#b]?-?\d+):([^*]+)\*(\d+)$/);
        if (!m) continue;
        tokens.push({
            note:    m[1],
            midi:    noteToMidi(m[1]),
            syllable: m[2],          // e.g. "a-", "-ma-", "-zing"
            // Display form: strip leading/trailing hyphens so the user
            // sees "a", "ma", "zing" instead of "-ma-".
            display: m[2].replace(/^-|-$/g, ""),
            beats:   parseInt(m[3], 10),
        });
    }
}

// Convert tokens to manifest notes with cumulative startSec + (bar,beat).
const notes = [];
let cumBeats = 0;
for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i];
    const bar  = Math.floor(cumBeats / 3);
    const beat = cumBeats % 3;
    const id   = `${i}`;            // simple linear ID for syllables
    const takePath = resolve(TAKES, `${id}.wav`);
    notes.push({
        id,
        bar,
        beat,
        midi: t.midi,
        note: t.note,
        word: t.display,
        durBeats: t.beats,
        durSec: t.beats * BEAT_SEC,
        startSec: cumBeats * BEAT_SEC,
        vowel: t.display,
        hasTake: existsSync(takePath),
    });
    cumBeats += t.beats;
}

const manifest = {
    track: "amazing",
    lane: "big-pictures",
    title: "Amazing Grace — verse 1",
    bpm: BPM,
    beatSec: BEAT_SEC,
    barSec: BAR_SEC,
    scale: "G major pentatonic",
    range: "D3-D4 (one octave + a tone)",
    lyric: "amazing grace / how sweet the sound / that saved a wretch like me / " +
           "i once was lost / but now am found / was blind but now i see",
    note_on_vowel: "the `vowel` field carries the SYLLABLE to sing — rfa.mjs prints it verbatim",
    notes,
};

mkdirSync(TAKES, { recursive: true });
writeFileSync(OUT, JSON.stringify(manifest, null, 2));
console.log(`✓ wrote ${OUT.replace(POP + "/", "pop/")}`);
console.log(`  ${notes.length} syllables, total ${cumBeats} beats ` +
            `(${(cumBeats * BEAT_SEC).toFixed(2)} s at ${BPM} bpm 3/4)`);
console.log(`  recorded so far: ${notes.filter(n => n.hasTake).length}/${notes.length}`);
console.log(`\nto record:  node pop/bin/rfa.mjs --track big-pictures`);
console.log(`  --status      see which syllables are missing`);
console.log(`  --only <id>   (re-)record one syllable (id = 0..${notes.length - 1})`);
