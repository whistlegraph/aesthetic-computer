#!/usr/bin/env node
// realign-amazing.mjs — force-align the jeffrey-pvc vocal stem to the
// amazing-grace score grid.
//
// The existing big-pictures cli.mjs score-stretch step uses rubberband
// 8× ceiling, which silently caps very long stretches. Result: the long
// hold notes (*5 in amazing.np) are not held long enough, and by the
// end of verse 1 the vocal drifts ~25 s ahead of the score. This script
// fixes that by slicing per-syllable and stretching each slice exactly
// to its target duration.
//
// Pipeline:
//   1 · parse amazing.np   → 28 syllable tokens (midi, syllable, beats)
//   2 · group syllables    → 26 runs (handles "a-/-ma-/-zing" → "amazing")
//   3 · read word alignment from amazing-vocal-words.json
//   4 · for each syllable:
//        • slice the corresponding portion of amazing-pitched.mp3
//        • rubberband --time RATIO to target durBeats * beatSec
//        • write to /tmp/amazing-realign/sNN.wav
//   5 · concat all syllables via ffmpeg adelay + amix at startSec
//   6 · output → pop/big-pictures/out/amazing-realigned.wav
//
// Use:  node pop/big-pictures/bin/realign-amazing.mjs
// Then: bake-c.mjs --voice realigned   (picks it up as the vocal stem)

import { execSync } from "node:child_process";
import { readFileSync, mkdirSync, existsSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const POP  = resolve(HERE, "../..");
const OUT_DIR = resolve(POP, "big-pictures/out");
const TMP_DIR = "/tmp/amazing-realign";
mkdirSync(TMP_DIR, { recursive: true });

const BPM = 70;
const beatSec = 60 / BPM;       // 0.857
const SOURCE = resolve(OUT_DIR, "amazing-pitched.mp3");
const WORDS  = resolve(OUT_DIR, "amazing-vocal-words.json");
const SCORE  = resolve(POP, "big-pictures/amazing.np");
const FINAL  = resolve(OUT_DIR, "amazing-realigned.wav");

for (const f of [SOURCE, WORDS, SCORE]) {
    if (!existsSync(f)) {
        console.error(`✗ missing input: ${f}`);
        process.exit(1);
    }
}

// ── 1) parse amazing.np ─────────────────────────────────────────────
function noteToMidi(s) {
    const m = s.match(/^([A-G])(#|b)?(-?\d+)$/);
    if (!m) throw new Error(`bad note: ${s}`);
    const pc = { C:0, D:2, E:4, F:5, G:7, A:9, B:11 }[m[1]];
    const acc = m[2] === "#" ? 1 : m[2] === "b" ? -1 : 0;
    return 12 * (parseInt(m[3], 10) + 1) + pc + acc;
}
const tokens = [];
{
    let cumBeats = 0;
    for (const raw of readFileSync(SCORE, "utf8").split("\n")) {
        const line = raw.split("#")[0].trim();
        if (!line || /^verse\b/i.test(line)) continue;
        for (const tok of line.split(/\s+/)) {
            const m = tok.match(/^([A-G][#b]?-?\d+):([^*]+)\*(\d+)$/);
            if (!m) continue;
            const syllable = m[2];
            tokens.push({
                note: m[1], midi: noteToMidi(m[1]),
                syllable,
                display: syllable.replace(/^-|-$/g, ""),
                startsHyphen: syllable.startsWith("-"),
                endsHyphen: syllable.endsWith("-"),
                beats: parseInt(m[3], 10),
                startSec: cumBeats * beatSec,
            });
            cumBeats += parseInt(m[3], 10);
        }
    }
}
console.log(`parsed ${tokens.length} syllables from amazing.np ` +
            `(${tokens.reduce((s,t) => s + t.beats, 0)} beats total)`);

// ── 2) group syllables into runs (continuations marked by leading "-")
const runs = [];
let current = null;
for (let i = 0; i < tokens.length; i++) {
    const t = tokens[i];
    if (!t.startsHyphen) {
        if (current) runs.push(current);
        current = { tokens: [t] };
    } else {
        if (!current) throw new Error("orphan hyphen-leading token");
        current.tokens.push(t);
    }
}
if (current) runs.push(current);
console.log(`grouped into ${runs.length} runs`);

// ── 3) read word alignment ──────────────────────────────────────────
const words = JSON.parse(readFileSync(WORDS, "utf8"));
console.log(`loaded ${words.length} word timings from amazing-vocal-words.json`);
if (words.length !== runs.length) {
    console.error(`✗ run/word count mismatch: ${runs.length} runs vs ${words.length} words`);
    process.exit(1);
}

// ── 4) slice + stretch each syllable ────────────────────────────────
const stems = [];
let synIdx = 0;
for (let r = 0; r < runs.length; r++) {
    const run = runs[r];
    const word = words[r];
    const wordDurMs = word.toMs - word.fromMs;
    const totalRunBeats = run.tokens.reduce((s, t) => s + t.beats, 0);
    let beatOffset = 0;
    for (const t of run.tokens) {
        const sliceStartMs = word.fromMs + (beatOffset / totalRunBeats) * wordDurMs;
        const sliceEndMs   = word.fromMs + ((beatOffset + t.beats) / totalRunBeats) * wordDurMs;
        const sliceDurS = (sliceEndMs - sliceStartMs) / 1000;
        const targetDurS = t.beats * beatSec;
        const ratio = targetDurS / sliceDurS;

        const id = synIdx.toString().padStart(2, "0");
        const sliceWav   = `${TMP_DIR}/s${id}-slice.wav`;
        const stretchWav = `${TMP_DIR}/s${id}.wav`;

        // Extract slice from amazing-pitched.mp3
        execSync(`ffmpeg -y -loglevel error -ss ${sliceStartMs/1000} ` +
                 `-t ${sliceDurS} -i "${SOURCE}" -ar 48000 -ac 1 ` +
                 `-c:a pcm_s16le "${sliceWav}"`);

        // Stretch via rubberband CLI. --crisp 1 = optimized for held vowels
        // (we're stretching sustained syllables, not transients). --formant
        // preserves vocal character at extreme ratios.
        if (ratio > 1.03) {
            try {
                execSync(`rubberband --time ${ratio.toFixed(5)} --crisp 1 ` +
                         `--formant "${sliceWav}" "${stretchWav}" 2>/dev/null`);
            } catch (e) {
                console.error(`✗ rubberband failed on syllable ${id} ` +
                              `(${t.display}, ratio=${ratio.toFixed(2)})`);
                throw e;
            }
        } else {
            // Negligible stretch — just copy
            execSync(`cp "${sliceWav}" "${stretchWav}"`);
        }

        const actualDurS = parseFloat(execSync(
            `ffprobe -v error -show_entries format=duration -of csv=p=0 ` +
            `"${stretchWav}"`).toString().trim());

        stems.push({
            syllable: t.display,
            note: t.note,
            startSec: t.startSec,
            targetDurS,
            actualDurS,
            ratio,
            wavPath: stretchWav,
        });
        console.log(`  s${id} ${t.display.padEnd(8)} ${t.note.padEnd(3)} ` +
                    `slice ${sliceDurS.toFixed(2)}s → ` +
                    `stretch ${ratio.toFixed(2)}× → ` +
                    `target ${targetDurS.toFixed(2)}s ` +
                    `(actual ${actualDurS.toFixed(2)}s) @ ${t.startSec.toFixed(2)}s`);
        synIdx++;
        beatOffset += t.beats;
    }
}

// ── 5) concat all syllables via adelay + amix ───────────────────────
const TOTAL_S = tokens[tokens.length - 1].startSec +
                tokens[tokens.length - 1].beats * beatSec + 1.0;
const args = ["-y", "-loglevel", "error"];
for (const s of stems) args.push("-i", s.wavPath);
const filters = [];
for (let i = 0; i < stems.length; i++) {
    const ms = Math.round(stems[i].startSec * 1000);
    filters.push(`[${i}:a]adelay=${ms}|${ms}[d${i}]`);
}
filters.push(stems.map((_, i) => `[d${i}]`).join("") +
             `amix=inputs=${stems.length}:duration=longest:normalize=0,` +
             `atrim=duration=${TOTAL_S.toFixed(3)}[out]`);
args.push("-filter_complex", filters.join(";"), "-map", "[out]",
          "-ar", "48000", "-ac", "2", "-c:a", "pcm_s16le", FINAL);

console.log(`\n→ mixing ${stems.length} syllables into ${TOTAL_S.toFixed(2)}s output...`);
execSync(`ffmpeg ${args.map(a => `"${a}"`).join(" ")}`);

const outDurS = parseFloat(execSync(
    `ffprobe -v error -show_entries format=duration -of csv=p=0 "${FINAL}"`
).toString().trim());
console.log(`✓ wrote ${FINAL.replace(POP + "/", "pop/")} (${outDurS.toFixed(2)}s)`);
console.log(`\nuse via bake-c.mjs:`);
console.log(`  node pop/big-pictures/c/bake-c.mjs --voice realigned`);
