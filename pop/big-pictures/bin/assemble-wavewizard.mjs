#!/usr/bin/env node
// assemble-wavewizard.mjs — stitch wave-wizard takes into a vocal stem
// for amazing-grace, with EVERY take machined (auto-tuned + stretched)
// off the original so it sits on the score grid like the synth voices.
//
// For each note in wave-wizard/samples/amazing-grace/spec.json:
//
//   1. If `<spec.outDir>/<sample-name>-note-<i>.wav` exists, MACHINE it:
//        • detect rough f0 via autocorrelation on a mid-take window
//        • compute semitone shift to the target MIDI (hard snap)
//        • compute time-stretch ratio to target durBeats × beatSec
//        • rubberband --pitch SHIFT --time STRETCH --formant --crisp 5
//      The original take stays untouched on disk; the machined copy
//      lives at /tmp/wavewizard-machined/.
//
//   2. If the take is missing, fall back to the corresponding slice of
//      pop/big-pictures/out/amazing-realigned.wav (force-aligned jeffrey-pvc
//      from realign-amazing.mjs) so the song stays sung end-to-end.
//
//   3. Schedule the chosen source at its score `startSec` via ffmpeg
//      `adelay + amix`. Output: pop/big-pictures/out/amazing-wavewizard.wav
//      (~55 s, 48 kHz stereo).
//
// Then:
//   node pop/big-pictures/c/bake-c.mjs --voice wavewizard
// uses this as the vocal layer over the C bed.

import { execSync } from "node:child_process";
import { readFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const POP  = resolve(REPO, "pop");
const SPEC = resolve(REPO, "wave-wizard/samples/amazing-grace/spec.json");
const REALIGNED = resolve(POP, "big-pictures/out/amazing-realigned.wav");
const OUT  = resolve(POP, "big-pictures/out/amazing-wavewizard.wav");
const TMP  = "/tmp/wavewizard-machined";
mkdirSync(TMP, { recursive: true });

// WORLD vocoder for pitch lock (pop/.venv has pyworld + soundfile)
const WORLD_PY  = resolve(POP, ".venv/bin/python");
const PITCHSNAP = resolve(POP, "bin/pitchsnap_world.py");
const USE_WORLD = existsSync(WORLD_PY) && existsSync(PITCHSNAP);
function midiToName(m) {
    const NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
    const oct = Math.floor(m / 12) - 1;
    return NAMES[m % 12] + oct;
}

const BPM = 70;
const beatSec = 60 / BPM;
const TARGET_DUR = 64 * beatSec + 1.0;

if (!existsSync(SPEC)) { console.error(`✗ spec not found: ${SPEC}`); process.exit(1); }
const spec = JSON.parse(readFileSync(SPEC, "utf8"));
const hasRealigned = existsSync(REALIGNED);
if (!hasRealigned) {
    console.warn(`⚠  ${REALIGNED.replace(POP + "/", "pop/")} missing — no fallback for unrecorded notes`);
    console.warn(`   run: node pop/big-pictures/bin/realign-amazing.mjs`);
}

// ── tiny WAV reader (16-bit PCM, mono or stereo → mono) ─────────────
function readWavMono(path) {
    const buf = readFileSync(path);
    if (buf.toString("ascii", 0, 4) !== "RIFF") throw new Error(`not a WAV: ${path}`);
    let p = 12, sampleRate = 0, channels = 1, bps = 16, dataStart = -1, dataSize = 0;
    while (p < buf.length - 8) {
        const id = buf.toString("ascii", p, p + 4);
        const size = buf.readUInt32LE(p + 4);
        if (id === "fmt ") {
            channels = buf.readUInt16LE(p + 10);
            sampleRate = buf.readUInt32LE(p + 12);
            bps = buf.readUInt16LE(p + 22);
        } else if (id === "data") {
            dataStart = p + 8; dataSize = size; break;
        }
        p += 8 + size;
    }
    if (dataStart < 0 || bps !== 16) throw new Error(`unsupported WAV: ${path}`);
    const bytesPerFrame = channels * 2;
    const frames = Math.floor(dataSize / bytesPerFrame);
    const samples = new Float32Array(frames);
    for (let i = 0; i < frames; i++) {
        const off = dataStart + i * bytesPerFrame;
        const s = buf.readInt16LE(off);
        samples[i] = s / 32768;
    }
    return { samples, sampleRate, durationSec: frames / sampleRate };
}

// ── autocorrelation pitch detector ──────────────────────────────────
// Picks a centered ~0.6 s window from the take and finds the lag that
// maximises r[lag] / sqrt(e[0] * e[lag]). Range capped to vocal pitches
// (D2..D5 = 73..587 Hz) so we ignore noise / partials outside that band.
function detectPitchHz(samples, sr, minHz = 73, maxHz = 587) {
    const minLag = Math.floor(sr / maxHz);
    const maxLag = Math.ceil(sr / minHz);
    const dur = samples.length / sr;
    const winSec = Math.min(0.6, Math.max(0.15, dur - 0.2));
    const startSec = Math.max(0.05, (dur - winSec) / 2);
    const start = Math.floor(startSec * sr);
    const end = Math.min(samples.length, start + Math.floor(winSec * sr));
    const W = end - start;
    if (W < maxLag * 2) return null;
    // Remove DC
    let mean = 0;
    for (let i = start; i < end; i++) mean += samples[i];
    mean /= W;
    // Pre-compute squared sum windowed for normalization
    let e0 = 0;
    for (let i = start; i < end; i++) { const v = samples[i] - mean; e0 += v * v; }
    if (e0 < 1e-6) return null;
    let bestLag = -1, bestR = -1;
    for (let lag = minLag; lag <= maxLag; lag++) {
        let r = 0, e1 = 0;
        for (let i = start; i + lag < end; i++) {
            const a = samples[i] - mean;
            const b = samples[i + lag] - mean;
            r += a * b;
            e1 += b * b;
        }
        const denom = Math.sqrt(e0 * e1);
        if (denom < 1e-9) continue;
        const nr = r / denom;
        if (nr > bestR) { bestR = nr; bestLag = lag; }
    }
    if (bestLag <= 0 || bestR < 0.3) return null;
    return sr / bestLag;
}
function hzToMidi(hz) { return 69 + 12 * Math.log2(hz / 440); }

// ── walk spec; build per-note source list ───────────────────────────
const notes = [];
let cumBeats = 0;
for (const sample of spec.samples) {
    if (!sample.score) continue;
    for (let i = 0; i < sample.score.notes.length; i++) {
        const n = sample.score.notes[i];
        const startSec = cumBeats * beatSec;
        const durSec = n.beats * beatSec;
        const targetMidi = sample.score.rootMel + n.off;
        const takePath = `${sample.outDir ?? spec.outDir}/${sample.name}-note-${i}.wav`;
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
console.log(`spec walked: ${notes.length} notes (${cumBeats}β / ${(cumBeats * beatSec).toFixed(2)}s)\n`);

// ── for each note, MACHINE the take or extract realigned slice ─────
const stems = [];
let machined = 0, fallback = 0, skipped = 0;
for (const note of notes) {
    const id = note.globalIdx.toString().padStart(2, "0");
    if (note.takePath) {
        // MACHINE the recorded take. Two-step:
        //   1. WORLD vocoder f0 replacement → locks per-frame pitch to
        //      the target note regardless of the octave the user sang.
        //      The script measures the take's actual f0 contour with
        //      harvest+stonemask, replaces it with the target, then
        //      resynthesizes preserving spectral envelope (formants =
        //      voice character) and aperiodicity (breath / consonants).
        //   2. rubberband --time STRETCH --formant → snap duration to
        //      target durBeats × beatSec. No pitch change here — WORLD
        //      already handled it.
        const wav = readWavMono(note.takePath);
        const stretch = note.durSec / wav.durationSec;
        const targetName = midiToName(note.targetMidi);
        const pitchedPath = `${TMP}/${note.sampleName}-${id}-pitched.wav`;
        const machinedPath = `${TMP}/${note.sampleName}-${id}.wav`;

        // Log raw detected f0 just for visibility; not used for shifting.
        const detectedHz = detectPitchHz(wav.samples, wav.sampleRate);
        const detectMsg = detectedHz
            ? `sang ~${hzToMidi(detectedHz).toFixed(1)}`
            : "sang ?";

        let pitchSrc;
        if (USE_WORLD) {
            try {
                execSync(`"${WORLD_PY}" "${PITCHSNAP}" ` +
                         `"${note.takePath}" "${pitchedPath}" ` +
                         `--notes "${targetName}" --xfade-ms 0 ` +
                         `--voicing-ramp-ms 20`,
                         { stdio: ["ignore", "ignore", "pipe"] });
                pitchSrc = pitchedPath;
            } catch (e) {
                console.error(`  ${id} WORLD failed on ${note.sampleName}-${note.noteIdx}: ` +
                              `${(e.stderr || "").toString().split("\n")[0]}`);
                pitchSrc = note.takePath;  // raw take, no pitch lock
            }
        } else {
            pitchSrc = note.takePath;
        }

        const rubberPath = `${TMP}/${note.sampleName}-${id}-rubber.wav`;
        try {
            execSync(`rubberband --time ${stretch.toFixed(4)} ` +
                     `--formant --crisp 5 ` +
                     `"${pitchSrc}" "${rubberPath}" 2>/dev/null`);
        } catch (e) {
            console.error(`  ${id} rubberband FAILED on ${note.sampleName}-${note.noteIdx}: ${e.message}`);
            skipped++; continue;
        }
        // PER-TAKE LOUDNESS NORMALIZATION — bring every machined take to
        // the same target LUFS so the vocal stem is machine-uniform like
        // the synth voice (no take-to-take volume swings).
        try {
            execSync(`ffmpeg -y -loglevel error -i "${rubberPath}" ` +
                     `-af "loudnorm=I=-18:TP=-1.5:LRA=4" ` +
                     `-ar 48000 -ac 2 -c:a pcm_s16le "${machinedPath}"`);
        } catch (e) {
            // If loudnorm fails (e.g. silent slice), just use the rubberband output
            execSync(`cp "${rubberPath}" "${machinedPath}"`);
        }
        stems.push({ path: machinedPath, startSec: note.startSec, durSec: note.durSec });
        machined++;
        const tag = USE_WORLD ? "WORLD→" + targetName : "raw→" + targetName;
        console.log(`  ${id} ${note.sampleName.padEnd(7)} #${note.noteIdx} ` +
                    `${tag.padEnd(12)} (${detectMsg})  ` +
                    `stretch ${stretch.toFixed(2)}× @ ${note.startSec.toFixed(2)}s`);
    } else if (hasRealigned) {
        // FALLBACK: extract slice from amazing-realigned.wav
        const slicePath = `${TMP}/fb-${id}.wav`;
        execSync(`ffmpeg -y -loglevel error -ss ${note.startSec} ` +
                 `-t ${note.durSec.toFixed(3)} -i "${REALIGNED}" ` +
                 `-ar 48000 -ac 2 -c:a pcm_s16le "${slicePath}"`);
        stems.push({ path: slicePath, startSec: note.startSec, durSec: note.durSec });
        fallback++;
        console.log(`  ${id} ${note.sampleName.padEnd(7)} #${note.noteIdx} ` +
                    `fallback (realigned slice) @ ${note.startSec.toFixed(2)}s`);
    } else {
        skipped++;
        console.log(`  ${id} ${note.sampleName.padEnd(7)} #${note.noteIdx} ` +
                    `SKIP (no take, no fallback)`);
    }
}
console.log(`\n  machined: ${machined}  ·  fallback: ${fallback}  ·  skipped: ${skipped}`);
if (stems.length === 0) {
    console.error(`\n✗ nothing to stitch — record some takes or run realign-amazing.mjs`);
    process.exit(1);
}

// ── stitch via ffmpeg adelay + amix ─────────────────────────────────
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
             `atrim=duration=${TARGET_DUR.toFixed(3)}[out]`);
const STEM_PRE = OUT.replace(/\.wav$/, "-pre.wav");
args.push("-filter_complex", filters.join(";"),
          "-map", "[out]", "-ar", "48000", "-ac", "2",
          "-c:a", "pcm_s16le", STEM_PRE);
console.log(`\n→ stitching ${stems.length} stems into ${TARGET_DUR.toFixed(2)}s output...`);
execSync(`ffmpeg ${args.map(a => `"${a}"`).join(" ")}`);

// FINAL STEM LOUDNESS MATCH — bring the assembled vocal up to the C
// bed's integrated LUFS so it sits alongside the synth voice at equal
// perceived loudness. Bed measured at -12.3 LUFS; target -12 here.
// LRA=4 keeps the stem tight (the bed runs at LRA 3.1).
console.log(`→ loudness-matching stem to bed (target -12 LUFS, LRA 4)...`);
execSync(`ffmpeg -y -loglevel error -i "${STEM_PRE}" ` +
         `-af "loudnorm=I=-12:TP=-1.0:LRA=4" ` +
         `-ar 48000 -ac 2 -c:a pcm_s16le "${OUT}"`);
const outDur = parseFloat(execSync(
    `ffprobe -v error -show_entries format=duration -of csv=p=0 "${OUT}"`
).toString().trim());
console.log(`\n✓ wrote ${OUT.replace(POP + "/", "pop/")} (${outDur.toFixed(2)}s)`);
console.log(`\n  node pop/big-pictures/c/bake-c.mjs --voice wavewizard`);
