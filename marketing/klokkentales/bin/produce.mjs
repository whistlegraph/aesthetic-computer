#!/usr/bin/env node

import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";
import sharp from "sharp";
import { renderJingles, renderSineBed } from "../../podcast/bin/jingle.mjs";
import { master } from "../../podcast/bin/master.mjs";
import { loadKlokkentalesEnv } from "../lib/env.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const ROOT = resolve(HERE, "..");
const OUT = resolve(ROOT, "out");
const CACHE = resolve(OUT, "cache");
const argv = process.argv.slice(2);
const slug = argv.find((arg) => !arg.startsWith("--"));
const has = (flag) => argv.includes(`--${flag}`);
const env = loadKlokkentalesEnv();

if (!slug) {
  console.error("usage: node bin/produce.mjs <episode-slug> [--dry-run] [--placeholder-prutti] [--force]");
  process.exit(1);
}

const episodePath = resolve(ROOT, "episodes", `${slug}.mjs`);
if (!existsSync(episodePath)) throw new Error(`episode not found: ${episodePath}`);
const { episode } = await import(pathToFileURL(episodePath));

const roles = new Set(["jeffrey", "prutti"]);
if (!episode.lines?.length) throw new Error("episode has no lines");
for (const [index, line] of episode.lines.entries()) {
  if (!roles.has(line.speaker)) throw new Error(`line ${index + 1}: unknown speaker ${line.speaker}`);
  if (!line.text?.trim()) throw new Error(`line ${index + 1}: empty text`);
}

const words = episode.lines.reduce((sum, line) => sum + line.text.trim().split(/\s+/).length, 0);
const pruttiMode = env.PRUTTI_ELEVENLABS_VOICE_ID
  ? "ElevenLabs voice"
  : has("placeholder-prutti")
    ? "generic casting placeholder"
    : "missing consented IVC";

console.log(`${episode.title}`);
console.log(`${episode.lines.length} lines · ${words} words · Jeffrey PVC + Prutti ${pruttiMode}`);
console.log(episode.syntheticVoiceDisclosure);
if (has("dry-run")) process.exit(0);

if (!env.PRUTTI_ELEVENLABS_VOICE_ID && !has("placeholder-prutti")) {
  throw new Error(
    "Prutti voice unavailable. Stage approved audio and run bin/voice.mjs create-ivc --confirm-rights-and-consent; or use --placeholder-prutti for a non-identity casting draft.",
  );
}

mkdirSync(CACHE, { recursive: true });
mkdirSync(OUT, { recursive: true });
const force = has("force");
const sayEndpoint = process.env.SAY_ENDPOINT || "https://aesthetic.computer/api/say";

async function postAudio(url, init, label) {
  let lastError;
  for (let attempt = 1; attempt <= 4; attempt++) {
    try {
      const response = await fetch(url, { ...init, signal: AbortSignal.timeout(60_000) });
      if (!response.ok) throw new Error(`${label} HTTP ${response.status}: ${(await response.text()).slice(0, 240)}`);
      return Buffer.from(await response.arrayBuffer());
    } catch (error) {
      lastError = error;
      if (attempt < 4) await new Promise((done) => setTimeout(done, 800 * attempt));
    }
  }
  throw lastError;
}

async function synthesize(line, index) {
  const sharedVoiceId = env.PRUTTI_ELEVENLABS_VOICE_ID || null;
  const spec = line.speaker === "jeffrey"
    ? { provider: "jeffrey", voice: "neutral:0", stability: 0.5, similarity: 0.86, speed: 0.96 }
    : sharedVoiceId
      ? { provider: "eleven-direct", voice: sharedVoiceId, stability: 0.38, similarity: 0.9, style: 0.48, speed: 0.98 }
      : { provider: "eleven", voice: "male:1", placeholder: true };
  const synthesisTarget = spec.provider === "eleven-direct"
    ? "elevenlabs:v1:text-to-speech:eleven_multilingual_v2"
    : sayEndpoint;
  const hash = createHash("sha256")
    .update(JSON.stringify({ text: line.text, speaker: line.speaker, spec, synthesisTarget }))
    .digest("hex")
    .slice(0, 20);
  const cached = resolve(CACHE, `${hash}.mp3`);
  if (!force && existsSync(cached) && readFileSync(cached).length) {
    console.log(`  · ${index + 1}/${episode.lines.length} ${line.speaker} cached`);
    return cached;
  }

  console.log(`  → ${index + 1}/${episode.lines.length} ${line.speaker}`);
  let audio;
  if (spec.provider === "eleven-direct") {
    if (!env.ELEVENLABS_API_KEY) throw new Error("ELEVENLABS_API_KEY is required for the Prutti voice");
    audio = await postAudio(
      `https://api.elevenlabs.io/v1/text-to-speech/${encodeURIComponent(spec.voice)}`,
      {
        method: "POST",
        headers: {
          "xi-api-key": env.ELEVENLABS_API_KEY,
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          text: line.text,
          model_id: "eleven_multilingual_v2",
          voice_settings: {
            stability: spec.stability,
            similarity_boost: spec.similarity,
            style: spec.style,
            use_speaker_boost: true,
            speed: spec.speed,
          },
        }),
      },
      "ElevenLabs Prutti PVC",
    );
  } else {
    audio = await postAudio(
      sayEndpoint,
      {
        method: "POST",
        redirect: "follow",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ from: line.text, ...spec }),
      },
      `/api/say ${line.speaker}`,
    );
  }
  writeFileSync(cached, audio);
  return cached;
}

const duration = (file) => Number(execFileSync("ffprobe", [
  "-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", file,
]).toString().trim());

function silence(file, seconds) {
  execFileSync("ffmpeg", [
    "-y", "-f", "lavfi", "-i", "anullsrc=r=44100:cl=stereo", "-t", String(seconds),
    "-c:a", "pcm_s16le", file,
  ], { stdio: "ignore" });
}

const clips = [];
for (let index = 0; index < episode.lines.length; index++) {
  clips.push(await synthesize(episode.lines[index], index));
}

const build = resolve(OUT, "build", slug);
rmSync(build, { recursive: true, force: true });
mkdirSync(build, { recursive: true });
const { intro, outro } = renderJingles(resolve(build, "jingles"));
const sequence = [];
const cues = [];
let clock = 0;
let sequenceIndex = 0;

function addAudio(source, cueText = null) {
  const target = resolve(build, `${String(sequenceIndex++).padStart(3, "0")}.wav`);
  const sourceDuration = duration(source);
  const fadeOutAt = Math.max(0, sourceDuration - 0.015).toFixed(3);
  execFileSync("ffmpeg", [
    "-y", "-i", source,
    "-af", `afade=t=in:st=0:d=0.008,afade=t=out:st=${fadeOutAt}:d=0.015`,
    "-ar", "44100", "-ac", "2", "-c:a", "pcm_s16le", target,
  ], { stdio: "ignore" });
  const clipDuration = duration(target);
  const start = clock;
  clock += clipDuration;
  sequence.push(target);
  if (cueText) cues.push({ start, end: clock, text: cueText });
}

function addSilence(seconds) {
  const target = resolve(build, `${String(sequenceIndex++).padStart(3, "0")}.wav`);
  silence(target, seconds);
  clock += seconds;
  sequence.push(target);
}

addAudio(intro);
addSilence(0.45);
for (let index = 0; index < clips.length; index++) {
  addAudio(clips[index], episode.lines[index].text);
  if (index < clips.length - 1) {
    const nextSameSpeaker = episode.lines[index + 1].speaker === episode.lines[index].speaker;
    addSilence(nextSameSpeaker ? 0.45 : 0.72);
  }
}
addSilence(0.5);
addAudio(outro);

const concatList = resolve(build, "concat.txt");
writeFileSync(concatList, sequence.map((file) => `file '${file.replaceAll("'", "'\\''")}'`).join("\n") + "\n");
const voice = resolve(build, "voice.wav");
execFileSync("ffmpeg", [
  "-y", "-f", "concat", "-safe", "0", "-i", concatList,
  "-c:a", "pcm_s16le", voice,
], { stdio: "ignore" });

const bed = resolve(build, "bed.wav");
renderSineBed(duration(voice) + 1, bed, { melody: true, melodyRestBars: 1 });
const premaster = resolve(build, "premaster.wav");
execFileSync("ffmpeg", [
  "-y", "-i", voice, "-i", bed,
  "-filter_complex",
  "[1:a]volume=0.13,highpass=f=55,lowpass=f=7200[bed];[bed][0:a]sidechaincompress=threshold=0.025:ratio=10:attack=8:release=600[ducked];[0:a][ducked]amix=inputs=2:duration=first:normalize=0[out]",
  "-map", "[out]", "-c:a", "pcm_s16le", premaster,
], { stdio: "ignore" });

const audioPath = resolve(OUT, `${slug}.mp3`);
master(premaster, audioPath, "tape");

const coverPath = resolve(OUT, `${slug}-cover-1400.png`);
await sharp(resolve(ROOT, "assets", "cover.svg")).png().toFile(coverPath);

const tagged = resolve(build, "tagged.mp3");
execFileSync("ffmpeg", [
  "-y", "-i", audioPath, "-i", coverPath,
  "-map", "0:a", "-map", "1:v", "-c", "copy", "-id3v2_version", "3",
  "-metadata:s:v", "title=Cover", "-metadata:s:v", "comment=Cover (front)",
  "-disposition:v", "attached_pic",
  "-metadata", `title=${episode.title}`,
  "-metadata", "artist=Jeffrey & Prutti",
  "-metadata", "album=Klokkentales",
  "-metadata", "genre=Storytelling",
  "-metadata", `date=${episode.date}`,
  "-metadata", `comment=${episode.syntheticVoiceDisclosure}`,
  tagged,
], { stdio: "ignore" });
writeFileSync(audioPath, readFileSync(tagged));

const srtTime = (seconds) => {
  const ms = Math.max(0, Math.round(seconds * 1000));
  const h = String(Math.floor(ms / 3_600_000)).padStart(2, "0");
  const m = String(Math.floor((ms % 3_600_000) / 60_000)).padStart(2, "0");
  const s = String(Math.floor((ms % 60_000) / 1000)).padStart(2, "0");
  return `${h}:${m}:${s},${String(ms % 1000).padStart(3, "0")}`;
};
writeFileSync(resolve(OUT, `${slug}.srt`), cues.map((cue, index) =>
  `${index + 1}\n${srtTime(cue.start)} --> ${srtTime(cue.end)}\n${cue.text}\n`,
).join("\n"));

const totalDuration = Math.round(duration(audioPath));
writeFileSync(resolve(OUT, `${slug}.json`), JSON.stringify({
  slug,
  title: episode.title,
  date: episode.date,
  description: episode.description,
  disclosure: episode.syntheticVoiceDisclosure,
  cast: ["Jeffrey", "Prutti"],
  pruttiVoice: env.PRUTTI_ELEVENLABS_VOICE_ID ? "consented-elevenlabs-voice" : "generic-casting-placeholder",
  status: env.PRUTTI_ELEVENLABS_VOICE_ID ? "review" : "casting-draft",
  source: episode.source,
  durationSec: totalDuration,
  bytes: readFileSync(audioPath).length,
  audio: `${slug}.mp3`,
  cover: `${slug}-cover-1400.png`,
  pubDate: new Date().toUTCString(),
}, null, 2) + "\n");

rmSync(build, { recursive: true, force: true });
console.log(`\n${audioPath}`);
console.log(`${Math.floor(totalDuration / 60)}m ${String(totalDuration % 60).padStart(2, "0")}s · review the complete render before staging`);
