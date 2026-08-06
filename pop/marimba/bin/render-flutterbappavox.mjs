#!/usr/bin/env node
// Flutterbappavox — full-song Jeffrey PVC singing through the YC video pipeline:
// timestamped speech → Flutterbap lead score → WORLD pitch → score stretch → vocal mix.

import { createHash } from "node:crypto";
import { mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve, relative } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const LANE = resolve(HERE, "..");
const POP = resolve(LANE, "..");
const ROOT = resolve(POP, "..");
const OUT = resolve(LANE, "out");
const SR = 48_000;
const BPM = 124;
const BEAT = 60 / BPM;
const BAR = 4 * BEAT;
const INTRO_EXTENSION = 4 * BAR;
const LEFT_PERC_OFF = INTRO_EXTENSION - BAR + 0.5 * BEAT;
const RIGHT_PERC_OFF = INTRO_EXTENSION - BAR + 2.5 * BEAT;
const SIDE_FADE = 0.7 * BEAT;
const SCHEDULE_ONLY = process.argv.includes("--schedule-only");

const LYRICS = resolve(LANE, "flutterbappavox.sing.txt");
const SCORE = resolve(LANE, "flutterbappavox-vocal.np");
const BED = resolve(OUT, "flutterbap.mp3");
const SOURCE = resolve(OUT, "flutterbappavox-full-vocal.mp3");
const SOURCE_ALIGNMENT = `${SOURCE}.alignment.json`;
const WORDS = resolve(OUT, "flutterbappavox-full-vocal-words.json");
const VOCAL_MAP = resolve(OUT, "flutterbappavox-vocal-map.json");
const PITCHED = resolve(OUT, "flutterbappavox-full-pitched.mp3");
const PITCHED_ALIGNMENT = resolve(OUT, "flutterbappavox-full-pitched-alignment.json");
const SUNG = resolve(OUT, "flutterbappavox-full-sung.mp3");
const SUNG_WORDS = resolve(OUT, "flutterbappavox-full-sung-words.json");
const VOCAL_WAV = resolve(OUT, "flutterbappavox-vocal.wav");
const WAV = resolve(OUT, "flutterbappavox-AUDITION.wav");
const MP3 = resolve(OUT, "flutterbappavox.mp3");
const EVENTS = resolve(OUT, "flutterbappavox.events.json");
mkdirSync(OUT, { recursive: true });

function run(command, args, capture = false) {
  const r = spawnSync(command, args, {
    cwd: ROOT,
    stdio: capture ? ["ignore", "pipe", "pipe"] : "inherit",
    encoding: capture ? "utf8" : undefined,
    maxBuffer: 64 << 20,
  });
  if (r.status !== 0) throw new Error(`${command} failed (${r.status})\n${r.stderr || ""}`);
  return r;
}

// Same authored path as marketing/talking-head's YC application song.
if (!SCHEDULE_ONLY) {
  run("node", ["pop/bin/say.mjs", relative(ROOT, LYRICS), "--timestamps",
    "--stability", "0.6", "--similarity", "0.9", "--out", relative(ROOT, SOURCE)]);
}
if (!SCHEDULE_ONLY) {
  run("node", ["pop/marimba/bin/compose-flutterbappavox-score.mjs", relative(ROOT, SOURCE_ALIGNMENT),
    "--out", relative(ROOT, SCORE), "--words", relative(ROOT, WORDS),
    "--map", relative(ROOT, VOCAL_MAP), "--marimba-score", "pop/marimba/flutterbap.np"]);
  run("node", ["pop/bin/score-pitch.mjs", "--slug", "flutterbappavox", "--section", "verse",
    "--transpose", "0", "--vocal", relative(ROOT, SOURCE), "--words", relative(ROOT, WORDS),
    "--score", relative(ROOT, SCORE), "--out", relative(ROOT, PITCHED)]);
  run("node", ["pop/bin/score-stretch.mjs", "--slug", "flutterbappavox", "--section", "verse",
    "--bpm", String(BPM), "--max-stretch", "6", "--onset-shift-ms", "60", "--overlap-ms", "35",
    "--in", relative(ROOT, PITCHED), "--alignment", relative(ROOT, PITCHED_ALIGNMENT),
    "--score", relative(ROOT, SCORE), "--out", relative(ROOT, SUNG)]);
}
run("node", ["pop/bin/score-stretch.mjs", "--slug", "flutterbappavox", "--section", "verse",
  "--bpm", String(BPM), "--score", relative(ROOT, SCORE), "--words-only", relative(ROOT, SUNG_WORDS)]);

const sungWords = JSON.parse(readFileSync(SUNG_WORDS, "utf8"));
const vocalMap = JSON.parse(readFileSync(VOCAL_MAP, "utf8"));
const vocalDurationSec = sungWords.at(-1)?.toMs / 1000 || 0;
const eventReceipt = sungWords.map((w, index) => ({
  index, text: w.text, startSec: w.fromMs / 1000, durationSec: (w.toMs - w.fromMs) / 1000,
  melody: vocalMap.words[index].segments.map((s) => ({
    note: s.note, startSec: s.startBeat * BEAT, durationSec: s.durationBeats * BEAT,
  })),
}));

if (!SCHEDULE_ONLY) {
  run("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-i", SUNG,
    "-ar", String(SR), "-ac", "1", "-c:a", "pcm_s24le", VOCAL_WAV]);

  const chorus = "between(t,29,47)+between(t,60,78)+between(t,87,98)";
  const filter = [
    `[0:a]asplit=3[firstsrc][secondsrc][bodysrc]`,
    `[firstsrc]atrim=start=0:end=${INTRO_EXTENSION.toFixed(6)},asetpts=PTS-STARTPTS[first]`,
    `[secondsrc]atrim=start=0:end=${INTRO_EXTENSION.toFixed(6)},asetpts=PTS-STARTPTS,channelsplit=channel_layout=stereo[secondL][secondR]`,
    `[secondL]afade=t=out:st=${LEFT_PERC_OFF.toFixed(6)}:d=${SIDE_FADE.toFixed(6)}[leftOff]`,
    `[secondR]afade=t=out:st=${RIGHT_PERC_OFF.toFixed(6)}:d=${SIDE_FADE.toFixed(6)}[rightOff]`,
    `[leftOff][rightOff]join=inputs=2:channel_layout=stereo:map=0.0-FL|1.0-FR[second]`,
    `[bodysrc]atrim=start=${INTRO_EXTENSION.toFixed(6)},asetpts=PTS-STARTPTS[body]`,
    `[first][second][body]concat=n=3:v=0:a=1,volume=0.84[bed]`,
    `[1:a]highpass=f=90,lowpass=f=10500,acompressor=threshold=-22dB:ratio=3:attack=7:release=130,asplit=7[side][lead][dblL][dblR][hi][low][air]`,
    `[bed][side]sidechaincompress=threshold=0.018:ratio=8:attack=5:release=190:makeup=1[ducked]`,
    `[lead]volume=2.05,pan=stereo|c0=0.707*c0|c1=0.707*c0[vLead]`,
    `[dblL]adelay=24,volume=0.20,pan=stereo|c0=c0|c1=0.04*c0[vL]`,
    `[dblR]adelay=49,volume=0.17,pan=stereo|c0=0.04*c0|c1=c0[vR]`,
    `[hi]asetrate=${SR}*1.1892,aresample=${SR},atempo=0.8409,highpass=f=180,volume='0.20*(${chorus})':eval=frame,pan=stereo|c0=0.18*c0|c1=0.82*c0[vHi]`,
    `[low]asetrate=${SR}*0.8409,aresample=${SR},atempo=1.1892,lowpass=f=6500,volume='0.15*(${chorus})':eval=frame,pan=stereo|c0=0.82*c0|c1=0.18*c0[vLow]`,
    `[air]highpass=f=3900,aecho=0.72:0.45:73|149:0.20|0.11,volume=0.12[vAir]`,
    `[ducked][vLead][vL][vR][vHi][vLow][vAir]amix=inputs=7:duration=longest:normalize=0,highpass=f=25,lowpass=f=18500,afade=t=out:st=${(vocalDurationSec - 1.25).toFixed(3)}:d=1.25,atrim=duration=${vocalDurationSec.toFixed(3)},alimiter=limit=0.88:attack=5:release=65,loudnorm=I=-14:TP=-1.2:LRA=9[out]`,
  ].join(";");
  for (const [path, codec] of [[WAV, ["-c:a", "pcm_s24le"]], [MP3, ["-c:a", "libmp3lame", "-b:a", "320k"]]]) {
    run("ffmpeg", ["-y", "-hide_banner", "-loglevel", "error", "-i", BED, "-i", SUNG,
      "-filter_complex", filter, "-map", "[out]", "-ar", String(SR), "-ac", "2", ...codec,
      "-metadata", "title=flutterbappavox", "-metadata", "artist=Aesthetic Dot Computer", path]);
  }
}

function probe(path) {
  return JSON.parse(run("ffprobe", ["-v", "error", "-show_entries", "format=duration,size:stream=sample_rate,channels,bits_per_raw_sample,codec_name", "-of", "json", path], true).stdout);
}
function loudness(path) {
  const r = spawnSync("ffmpeg", ["-hide_banner", "-nostats", "-i", path, "-filter_complex", "ebur128=peak=true", "-f", "null", "-"], { encoding: "utf8", maxBuffer: 16 << 20 });
  const s = (r.stderr || "").slice((r.stderr || "").lastIndexOf("Summary:"));
  return {
    integratedLufs: Number(/I:\s*(-?[\d.]+) LUFS/.exec(s)?.[1]),
    loudnessRangeLu: Number(/LRA:\s*(-?[\d.]+) LU/.exec(s)?.[1]),
    truePeakDbfs: Number(/Peak:\s*(-?[\d.]+) dBFS/.exec(s)?.[1]),
  };
}

const receipt = {
  schema: "aesthetic.computer/pop-vocal-events/v2",
  track: "flutterbappavox", derivesFrom: "flutterbap", deterministicAfterTtsCache: true,
  lyrics: relative(ROOT, LYRICS), score: relative(ROOT, SCORE),
  transport: { bpm: BPM, vocalDurationSec, introExtensionBars: 4,
    introTransition: "left percussion off, then right", vocalCoverage: [0, vocalDurationSec] },
  voice: {
    provider: "jeffrey", voice: "neutral:0",
    pipeline: ["ElevenLabs character timestamps", "Flutterbap lead-event score", "WORLD f0 replacement", "per-word rubberband score stretch"],
    wordCount: eventReceipt.length,
    melodyFollowing: "exact Flutterbap lead pitch classes and note-change boundaries, octave-adjusted for Jeffrey",
  },
  events: eventReceipt,
  qc: SCHEDULE_ONLY ? null : {
    wav: { ...probe(WAV), ...loudness(WAV), sha256: createHash("sha256").update(readFileSync(WAV)).digest("hex") },
    mp3: { ...probe(MP3), ...loudness(MP3), sha256: createHash("sha256").update(readFileSync(MP3)).digest("hex") },
  },
};
writeFileSync(EVENTS, JSON.stringify(receipt, null, 2) + "\n");
console.log(`✓ ${eventReceipt.length} sung words · ${vocalDurationSec.toFixed(1)}s vocal coverage`);
if (!SCHEDULE_ONLY) {
  console.log(`✓ ${MP3}`);
  console.log(`✓ ${WAV}`);
  console.log(`✓ ${EVENTS}`);
}
