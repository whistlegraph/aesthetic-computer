#!/usr/bin/env node
// assemble-wavewizard-dance.mjs — repurpose the existing wave-wizard
// hymn takes for the 120 BPM 4/4 dance remix. No new recordings needed:
// same syllables, same MIDIs, just compressed to the tighter dance grid.
//
// Pipeline (per note):
//   1. WORLD f0 lock to target MIDI (same as hymn pipeline — but here
//      the target is the dance spec's score, which happens to match
//      the hymn since both target the same melody)
//   2. rubberband --time to the DANCE target durBeats × 0.5 s
//      (much shorter than the hymn's 0.857 s/beat)
//   3. per-take loudnorm I=-18 for uniform level
// Then amix all stems at their dance-spec startSec with adelay.
// Final stem loudnorm I=-12 to sit with the dance bed.
//
// Reads:
//   • wave-wizard/samples/amazing-grace-dance/spec.json   (target grid)
//   • pop/big-pictures/voice-takes/wavewizard/<sample>-note-<i>.wav
//                                                         (existing hymn takes)
// Writes:
//   • pop/big-pictures/out/amazing-wavewizard-dance.wav   (~25 s stem)

import { execSync } from "node:child_process";
import { readFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const POP  = resolve(REPO, "pop");
const SPEC = resolve(REPO, "wave-wizard/samples/amazing-grace-dance/spec.json");
// Pull takes from the HYMN dir — same syllables, just being re-targeted.
const TAKES_DIR = resolve(POP, "big-pictures/voice-takes/wavewizard");
const OUT  = resolve(POP, "big-pictures/out/amazing-wavewizard-dance.wav");
const TMP  = "/tmp/wavewizard-machined-dance";
mkdirSync(TMP, { recursive: true });

// 120 BPM dance grid
const BPM = 120;
const beatSec = 60 / BPM;            // 0.5 s
// Sum the dance spec's beats to compute total vocal duration.

// WORLD pitch correction
const WORLD_PY  = resolve(POP, ".venv/bin/python");
const PITCHSNAP = resolve(POP, "bin/pitchsnap_world.py");
const USE_WORLD = existsSync(WORLD_PY) && existsSync(PITCHSNAP);
function midiToName(m) {
    const NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
    const oct = Math.floor(m / 12) - 1;
    return NAMES[m % 12] + oct;
}

if (!existsSync(SPEC)) { console.error(`✗ spec not found: ${SPEC}`); process.exit(1); }
const spec = JSON.parse(readFileSync(SPEC, "utf8"));

// Walk dance spec → cumulative beat counter → per-note target.
const notes = [];
let cumBeats = 0;
for (const sample of spec.samples) {
    if (!sample.score) continue;
    for (let i = 0; i < sample.score.notes.length; i++) {
        const n = sample.score.notes[i];
        const startSec = cumBeats * beatSec;
        const durSec = n.beats * beatSec;
        const targetMidi = sample.score.rootMel + n.off;
        // Take WAV lives in the HYMN takes dir — same file names.
        const takePath = `${TAKES_DIR}/${sample.name}-note-${i}.wav`;
        notes.push({
            globalIdx: notes.length,
            sampleName: sample.name,
            noteIdx: i,
            startSec, durSec, targetMidi,
            takePath: existsSync(takePath) ? takePath : null,
        });
        cumBeats += n.beats;
    }
}
const TOTAL = cumBeats * beatSec + 0.5;
console.log(`dance spec: ${notes.length} notes / ${cumBeats}β / ${(cumBeats*beatSec).toFixed(2)}s @ ${BPM} BPM\n`);

// ── machine each take (WORLD → rubberband → loudnorm) ───────────────
const stems = [];
let machined = 0, skipped = 0;
for (const note of notes) {
    const id = note.globalIdx.toString().padStart(2, "0");
    if (!note.takePath) { skipped++; continue; }

    const targetName = midiToName(note.targetMidi);
    const pitchedPath = `${TMP}/${note.sampleName}-${id}-pitched.wav`;
    const rubberPath  = `${TMP}/${note.sampleName}-${id}-rubber.wav`;
    const machinedPath = `${TMP}/${note.sampleName}-${id}.wav`;

    // 1) WORLD pitch lock
    let pitchSrc = note.takePath;
    if (USE_WORLD) {
        try {
            execSync(`"${WORLD_PY}" "${PITCHSNAP}" ` +
                     `"${note.takePath}" "${pitchedPath}" ` +
                     `--notes "${targetName}" --xfade-ms 0 --voicing-ramp-ms 20`,
                     { stdio: ["ignore", "ignore", "pipe"] });
            pitchSrc = pitchedPath;
        } catch (e) {
            console.error(`  ${id} WORLD failed on ${note.sampleName}-${note.noteIdx}: ` +
                          `${(e.stderr || "").toString().split("\n")[0]}`);
        }
    }

    // 2) Get current dur of pitch-locked take + rubberband to dance target
    const wavDur = parseFloat(execSync(
        `ffprobe -v error -show_entries format=duration -of csv=p=0 "${pitchSrc}"`
    ).toString().trim());
    const stretch = note.durSec / wavDur;

    try {
        execSync(`rubberband --time ${stretch.toFixed(4)} ` +
                 `--formant --crisp 5 ` +
                 `"${pitchSrc}" "${rubberPath}" 2>/dev/null`);
    } catch (e) {
        console.error(`  ${id} rubberband failed: ${e.message}`);
        skipped++; continue;
    }

    // 3) Per-take loudnorm for uniform level
    try {
        execSync(`ffmpeg -y -loglevel error -i "${rubberPath}" ` +
                 `-af "loudnorm=I=-18:TP=-1.5:LRA=4" ` +
                 `-ar 48000 -ac 2 -c:a pcm_s16le "${machinedPath}"`);
    } catch (e) {
        execSync(`cp "${rubberPath}" "${machinedPath}"`);
    }

    stems.push({ path: machinedPath, startSec: note.startSec });
    machined++;
    console.log(`  ${id} ${note.sampleName.padEnd(7)} #${note.noteIdx} ` +
                `→ ${targetName.padEnd(3)} stretch ${stretch.toFixed(2)}× ` +
                `→ ${note.durSec.toFixed(2)}s @ ${note.startSec.toFixed(2)}s`);
}
console.log(`\n  machined: ${machined}  ·  skipped: ${skipped}\n`);
if (stems.length === 0) {
    console.error(`✗ no stems — check that hymn takes exist at ${TAKES_DIR}`);
    process.exit(1);
}

// ── stitch via adelay + amix, then loudnorm to bed level ────────────
const STEM_PRE = OUT.replace(/\.wav$/, "-pre.wav");
const args = ["-y", "-loglevel", "error"];
for (const s of stems) args.push("-i", s.path);
const filters = [];
for (let k = 0; k < stems.length; k++) {
    const ms = Math.round(stems[k].startSec * 1000);
    filters.push(`[${k}:a]aresample=48000,aformat=channel_layouts=stereo,` +
                 `adelay=${ms}|${ms}[d${k}]`);
}
filters.push(stems.map((_, k) => `[d${k}]`).join("") +
             `amix=inputs=${stems.length}:duration=longest:` +
             `dropout_transition=0:normalize=0,` +
             `atrim=duration=${TOTAL.toFixed(3)}[out]`);
args.push("-filter_complex", filters.join(";"),
          "-map", "[out]", "-ar", "48000", "-ac", "2",
          "-c:a", "pcm_s16le", STEM_PRE);
console.log(`→ stitching ${stems.length} stems into ${TOTAL.toFixed(2)}s output...`);
execSync(`ffmpeg ${args.map(a => `"${a}"`).join(" ")}`);
console.log(`→ loudness-match to bed (target -12 LUFS, LRA 4)...`);
execSync(`ffmpeg -y -loglevel error -i "${STEM_PRE}" ` +
         `-af "loudnorm=I=-12:TP=-1.0:LRA=4" ` +
         `-ar 48000 -ac 2 -c:a pcm_s16le "${OUT}"`);

const outDur = parseFloat(execSync(
    `ffprobe -v error -show_entries format=duration -of csv=p=0 "${OUT}"`
).toString().trim());
console.log(`\n✓ wrote ${OUT.replace(POP + "/", "pop/")} (${outDur.toFixed(2)}s)`);
console.log(`\n  node pop/big-pictures/c/bake-c-dance.mjs --voice wavewizard-dance`);
