#!/usr/bin/env node
// assemble-wavewizard-mary.mjs — stitch the Mary Had a Little Lamb
// wave-wizard takes into a cute siney lullaby.
//
// Same tech infra as assemble-wavewizard.mjs, but Mary is its OWN song
// (no Amazing Grace melody, no jeffrey-pvc fallback). For each note in
// wave-wizard/samples/mary-lamb/spec.json:
//
//   1. If the take exists, MACHINE it:
//        • WORLD vocoder f0 replacement → lock per-frame pitch to the
//          target MIDI (preserves formants = voice character)
//        • rubberband --time STRETCH --formant → snap duration to the
//          (slowed) lullaby grid
//        • loudnorm → uniform per-note loudness
//   2. If the take is MISSING (e.g. a skipped note), drop a soft sine
//        "la" at the target pitch so the lullaby stays whole.
//
// A music-box sine-bell bed (melody octave-up, twinkling L/R) plus a
// warm root drone are synthesized in-process and mixed under the vocal.
//
// Output: pop/big-pictures/out/mary-lullaby.wav (48 kHz stereo), opened
// in QuickTime when done.

import { execSync } from "node:child_process";
import { readFileSync, writeFileSync, existsSync, mkdirSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const REPO = resolve(HERE, "../../..");
const POP  = resolve(REPO, "pop");
const SPEC = resolve(REPO, "wave-wizard/samples/mary-lamb/spec.json");
const OUT  = resolve(POP, "big-pictures/out/mary-lullaby.wav");
const TMP  = "/tmp/wavewizard-mary";
mkdirSync(TMP, { recursive: true });
mkdirSync(dirname(OUT), { recursive: true });

const WORLD_PY  = resolve(POP, ".venv/bin/python");
const PITCHSNAP = resolve(POP, "bin/pitchsnap_world.py");
const USE_WORLD = existsSync(WORLD_PY) && existsSync(PITCHSNAP);

const SR  = 48000;
const BPM = 80;                 // slowed from the 100 bpm record tempo → dreamier
const beatSec = 60 / BPM;       // 0.75 s
const LINE_GAP_BEATS = 1.5;     // breathing room between lines
const TAIL_SEC = 2.5;

const NAMES = ["C","C#","D","D#","E","F","F#","G","G#","A","A#","B"];
const midiToName = (m) => NAMES[((m % 12) + 12) % 12] + (Math.floor(m / 12) - 1);
const midiToHz   = (m) => 440 * Math.pow(2, (m - 69) / 12);

if (!existsSync(SPEC)) { console.error(`✗ spec not found: ${SPEC}`); process.exit(1); }
const spec = JSON.parse(readFileSync(SPEC, "utf8"));

// ── tiny WAV reader (16-bit PCM → mono float) ───────────────────────
function readWavMono(path) {
    const buf = readFileSync(path);
    let p = 12, channels = 1, sampleRate = SR, dataStart = -1, dataSize = 0;
    while (p < buf.length - 8) {
        const id = buf.toString("ascii", p, p + 4);
        const size = buf.readUInt32LE(p + 4);
        if (id === "fmt ") { channels = buf.readUInt16LE(p + 10); sampleRate = buf.readUInt32LE(p + 12); }
        else if (id === "data") { dataStart = p + 8; dataSize = size; break; }
        p += 8 + size;
    }
    const bpf = channels * 2, frames = Math.floor(dataSize / bpf);
    const samples = new Float32Array(frames);
    for (let i = 0; i < frames; i++) samples[i] = buf.readInt16LE(dataStart + i * bpf) / 32768;
    return { samples, sampleRate, durationSec: frames / sampleRate };
}

// ── mono 16-bit WAV writer ──────────────────────────────────────────
function writeWavMono(path, s, sr) {
    const buf = Buffer.alloc(44 + s.length * 2);
    buf.write("RIFF", 0); buf.writeUInt32LE(36 + s.length * 2, 4); buf.write("WAVE", 8);
    buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20);
    buf.writeUInt16LE(1, 22); buf.writeUInt32LE(sr, 24); buf.writeUInt32LE(sr * 2, 28);
    buf.writeUInt16LE(2, 32); buf.writeUInt16LE(16, 34);
    buf.write("data", 36); buf.writeUInt32LE(s.length * 2, 40);
    for (let i = 0; i < s.length; i++)
        buf.writeInt16LE((Math.max(-1, Math.min(1, s[i])) * 32767) | 0, 44 + i * 2);
    writeFileSync(path, buf);
}

// ── word-shape trim: find the syllable's real onset→offset from the
// short-time RMS envelope and tighten to it, with a consonant pre-roll
// so plosives/fricatives ('l', 'fl', 'wh', 's') aren't clipped. This is
// what lands the attack on the beat instead of stretching dead air.
const PRE_ROLL_SEC = 0.045;   // keep this much before the energy onset
const TAIL_SEC_PAD = 0.060;   // keep this much after the energy offset
function wordShapeTrim(samples, sr) {
    const win = Math.floor(sr * 0.01);            // 10 ms RMS frames
    const env = [];
    let peak = 0;
    for (let i = 0; i < samples.length; i += win) {
        let sum = 0; const end = Math.min(i + win, samples.length);
        for (let j = i; j < end; j++) sum += samples[j] * samples[j];
        const r = Math.sqrt(sum / win);
        env.push(r); if (r > peak) peak = r;
    }
    if (peak < 1e-5) return { start: 0, end: samples.length }; // silent take
    const onThr  = peak * 0.10;   // onset: first frame clearly above floor
    const offThr = peak * 0.06;   // offset: last frame still voiced (lower)
    let on = -1, off = -1;
    for (let i = 0; i < env.length; i++) {
        if (env[i] > onThr && on < 0) on = i;
        if (env[i] > offThr) off = i;
    }
    if (on < 0) return { start: 0, end: samples.length };
    const start = Math.max(0, on * win - Math.floor(PRE_ROLL_SEC * sr));
    const end   = Math.min(samples.length, (off + 1) * win + Math.floor(TAIL_SEC_PAD * sr));
    return { start, end };
}

// ── walk spec → per-note schedule (with line gaps) ──────────────────
const notes = [];
let cumBeats = 0;
for (const sample of spec.samples) {
    if (!sample.score) continue;
    for (let i = 0; i < sample.score.notes.length; i++) {
        const n = sample.score.notes[i];
        const takePath = `${spec.outDir}/${sample.name}-note-${i}.wav`;
        notes.push({
            globalIdx: notes.length,
            sampleName: sample.name,
            noteIdx: i,
            startSec: cumBeats * beatSec,
            durSec: n.beats * beatSec,
            targetMidi: sample.score.rootMel + n.off,
            rootMel: sample.score.rootMel,
            lineStartSec: null,
            takePath: existsSync(takePath) ? takePath : null,
        });
        cumBeats += n.beats;
    }
    cumBeats += LINE_GAP_BEATS;
}
const SONG_SEC = notes[notes.length - 1].startSec + notes[notes.length - 1].durSec;
const TOTAL_SEC = SONG_SEC + TAIL_SEC;
console.log(`spec walked: ${notes.length} notes · ${SONG_SEC.toFixed(2)}s song @ ${BPM} bpm\n`);

// ── machine each existing take → vocal stems ────────────────────────
const stems = [];
let machined = 0, filled = 0;
for (const note of notes) {
    const id = note.globalIdx.toString().padStart(2, "0");
    const targetName = midiToName(note.targetMidi);
    if (!note.takePath) {
        console.log(`  ${id} ${note.sampleName.padEnd(7)} #${note.noteIdx}  ` +
                    `MISSING → soft sine "${targetName}" @ ${note.startSec.toFixed(2)}s`);
        note.fill = true; filled++;
        continue;
    }
    // 1. find the word's shape and tighten to it (drops leading breath /
    //    trailing silence so the syllable fills — not floats inside — the slot)
    const wav = readWavMono(note.takePath);
    const { start, end } = wordShapeTrim(wav.samples, wav.sampleRate);
    const tightPath = `${TMP}/${note.sampleName}-${id}-tight.wav`;
    writeWavMono(tightPath, wav.samples.subarray(start, end), wav.sampleRate);
    const tightDur = (end - start) / wav.sampleRate;
    const stretch = note.durSec / tightDur;
    const trimmedMs = Math.round((wav.durationSec - tightDur) * 1000);

    const pitchedPath  = `${TMP}/${note.sampleName}-${id}-pitched.wav`;
    const rubberPath   = `${TMP}/${note.sampleName}-${id}-rubber.wav`;
    const machinedPath = `${TMP}/${note.sampleName}-${id}.wav`;

    // 2. pitch-lock the tightened word to the target note
    let pitchSrc = tightPath;
    if (USE_WORLD) {
        try {
            execSync(`"${WORLD_PY}" "${PITCHSNAP}" "${tightPath}" "${pitchedPath}" ` +
                     `--notes "${targetName}" --xfade-ms 0 --voicing-ramp-ms 20`,
                     { stdio: ["ignore", "ignore", "pipe"] });
            pitchSrc = pitchedPath;
        } catch (e) {
            console.error(`  ${id} WORLD failed: ${(e.stderr || "").toString().split("\n")[0]}`);
        }
    }
    // 3. stretch the tightened word onto the grid slot
    execSync(`rubberband --time ${stretch.toFixed(4)} --formant --crisp 5 ` +
             `"${pitchSrc}" "${rubberPath}" 2>/dev/null`);
    try {
        execSync(`ffmpeg -y -loglevel error -i "${rubberPath}" ` +
                 `-af "loudnorm=I=-18:TP=-1.5:LRA=4" -ar ${SR} -ac 2 -c:a pcm_s16le "${machinedPath}"`);
    } catch { execSync(`cp "${rubberPath}" "${machinedPath}"`); }
    stems.push({ path: machinedPath, startSec: note.startSec });
    machined++;
    console.log(`  ${id} ${note.sampleName.padEnd(7)} #${note.noteIdx}  ` +
                `WORLD→${targetName.padEnd(3)} trim -${String(trimmedMs).padStart(3)}ms ` +
                `stretch ${stretch.toFixed(2)}× @ ${note.startSec.toFixed(2)}s`);
}
console.log(`\n  machined: ${machined}  ·  sine-filled: ${filled}`);

// ── synthesize the siney bed (sine bells + drone) + fill notes ──────
const N = Math.ceil(TOTAL_SEC * SR);
const L = new Float32Array(N);
const R = new Float32Array(N);
const TAU = 2 * Math.PI;

function addTone(startSec, durSec, hz, amp, pan, harmonics, attackSec, decayTau) {
    const s0 = Math.floor(startSec * SR);
    const len = Math.floor((durSec + decayTau * 3) * SR);
    const gL = Math.cos((pan + 1) * Math.PI / 4); // equal-power pan
    const gR = Math.sin((pan + 1) * Math.PI / 4);
    for (let i = 0; i < len; i++) {
        const idx = s0 + i;
        if (idx < 0 || idx >= N) continue;
        const t = i / SR;
        // attack ramp then exponential ring-down
        const atk = t < attackSec ? t / attackSec : 1;
        const env = atk * Math.exp(-Math.max(0, t - attackSec) / decayTau);
        let v = 0;
        for (let h = 0; h < harmonics.length; h++)
            v += harmonics[h] * Math.sin(TAU * hz * (h + 1) * t);
        v *= amp * env;
        L[idx] += v * gL;
        R[idx] += v * gR;
    }
}

// per-line warm root drone (soft sine an octave below the tonic + its octave)
const lineStarts = [];
{
    let cb = 0;
    for (const sample of spec.samples) {
        if (!sample.score) continue;
        const beats = sample.score.notes.reduce((a, n) => a + n.beats, 0);
        lineStarts.push({ start: cb * beatSec, dur: beats * beatSec, root: sample.score.rootMel });
        cb += beats + LINE_GAP_BEATS;
    }
}
for (const ln of lineStarts) {
    addTone(ln.start, ln.dur, midiToHz(ln.root - 12), 0.055, 0, [1, 0.25], 0.35, ln.dur * 0.9);
    addTone(ln.start, ln.dur, midiToHz(ln.root - 12 + 7), 0.03, 0, [1], 0.45, ln.dur * 0.9); // soft fifth
}

// per-note music-box sine bell (melody, octave up, twinkling L/R) +
// soft sine "la" fill for any missing vocal note
for (const note of notes) {
    const bellHz = midiToHz(note.targetMidi + 12);
    const pan = (note.globalIdx % 2 === 0) ? -0.35 : 0.35;
    const bellTau = Math.max(0.3, Math.min(note.durSec, 1.1)) * 0.7;
    addTone(note.startSec, note.durSec, bellHz, 0.12, pan, [1, 0.18, 0.06], 0.004, bellTau);

    if (note.fill) {
        // wordless vocal-range sine with a vowel-ish harmonic stack
        const hz = midiToHz(note.targetMidi);
        addTone(note.startSec, note.durSec, hz, 0.16, 0,
                [1, 0.5, 0.28, 0.12], 0.04, note.durSec * 0.8);
    }
}

// ── write bed to 16-bit stereo WAV ──────────────────────────────────
function writeWav(path, l, r) {
    const frames = l.length;
    const buf = Buffer.alloc(44 + frames * 4);
    buf.write("RIFF", 0); buf.writeUInt32LE(36 + frames * 4, 4); buf.write("WAVE", 8);
    buf.write("fmt ", 12); buf.writeUInt32LE(16, 16); buf.writeUInt16LE(1, 20);
    buf.writeUInt16LE(2, 22); buf.writeUInt32LE(SR, 24); buf.writeUInt32LE(SR * 4, 28);
    buf.writeUInt16LE(4, 32); buf.writeUInt16LE(16, 34);
    buf.write("data", 36); buf.writeUInt32LE(frames * 4, 40);
    let p = 44;
    for (let i = 0; i < frames; i++) {
        const cl = Math.max(-1, Math.min(1, l[i])), cr = Math.max(-1, Math.min(1, r[i]));
        buf.writeInt16LE((cl * 32767) | 0, p); buf.writeInt16LE((cr * 32767) | 0, p + 2);
        p += 4;
    }
    writeFileSync(path, buf);
}
const BED = `${TMP}/bed.wav`;
writeWav(BED, L, R);
console.log(`\n→ synthesized siney bed (${TOTAL_SEC.toFixed(2)}s)`);

// ── mix vocal stems + bed via ffmpeg ────────────────────────────────
const args = ["-y", "-loglevel", "error", "-i", BED];
for (const s of stems) args.push("-i", s.path);
const filters = [];
filters.push(`[0:a]aresample=${SR},aformat=channel_layouts=stereo,volume=1.0[bed]`);
for (let k = 0; k < stems.length; k++) {
    const ms = Math.round(stems[k].startSec * 1000);
    filters.push(`[${k + 1}:a]aresample=${SR},aformat=channel_layouts=stereo,` +
                 `volume=0.85,adelay=${ms}|${ms}[v${k}]`);
}
const mixIns = ["[bed]", ...stems.map((_, k) => `[v${k}]`)].join("");
filters.push(`${mixIns}amix=inputs=${stems.length + 1}:duration=longest:` +
             `dropout_transition=0:normalize=0[mix]`);
// gentle lullaby polish: soft high roll-off for warmth + a touch of dreamy echo
filters.push(`[mix]lowpass=f=9000,aecho=0.8:0.7:60|110:0.22|0.16,` +
             `atrim=duration=${TOTAL_SEC.toFixed(3)}[out]`);
args.push("-filter_complex", filters.join(";"), "-map", "[out]",
          "-ar", String(SR), "-ac", "2", "-c:a", "pcm_s16le", `${TMP}/mix.wav`);
console.log(`→ mixing ${stems.length} vocal notes over the bed...`);
execSync(`ffmpeg ${args.map(a => `"${a}"`).join(" ")}`);

// soft master — lullaby sits gentle, not loud
execSync(`ffmpeg -y -loglevel error -i "${TMP}/mix.wav" ` +
         `-af "loudnorm=I=-16:TP=-1.5:LRA=6" -ar ${SR} -ac 2 -c:a pcm_s16le "${OUT}"`);
const outDur = parseFloat(execSync(
    `ffprobe -v error -show_entries format=duration -of csv=p=0 "${OUT}"`).toString().trim());
console.log(`\n✓ wrote ${OUT.replace(POP + "/", "pop/")} (${outDur.toFixed(2)}s)`);

// ── open in QuickTime ───────────────────────────────────────────────
execSync(`open -a "QuickTime Player" "${OUT}"`);
console.log(`→ opened in QuickTime Player 🐑`);
