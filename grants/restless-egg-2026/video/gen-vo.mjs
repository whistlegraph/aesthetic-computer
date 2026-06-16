#!/usr/bin/env node
// gen-vo.mjs — synthesize the Restless Egg intro VO in Jeffrey's PVC voice.
// Writes vo.mp3 + vo.wav (48k stereo) + vo.json (word-level timestamps for
// caption sync and shot alignment).
//
// Run:  node grants/restless-egg-2026/video/gen-vo.mjs
// Reads ELEVENLABS_API_KEY from the AC vault env.

import { readFileSync, writeFileSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const VOICE_ID = process.env.VOICE_ID || "ZXoQQp5X0PKHGwyZpVIT"; // @jeffrey PVC (re-cloned; old id dYNGZ... is dead)
const MODEL_ID = process.env.MODEL_ID || "eleven_multilingual_v2";
const VAULT_ENV =
  "/Users/jas/aesthetic-computer/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env";

function loadKey() {
  if (process.env.ELEVENLABS_API_KEY) return process.env.ELEVENLABS_API_KEY;
  return readFileSync(VAULT_ENV, "utf8")
    .split("\n").find((l) => l.startsWith("ELEVENLABS_API_KEY="))
    .split("=", 2)[1].trim();
}

async function tts(apiKey, text, outBase) {
  const url = `https://api.elevenlabs.io/v1/text-to-speech/${VOICE_ID}/with-timestamps?output_format=mp3_44100_128`;
  const body = {
    text,
    model_id: MODEL_ID,
    // stability 0.5 keeps Jeffrey's identity (per jeffrey-pvc voice notes);
    // style adds a little life without drifting off-voice.
    voice_settings: {
      stability: Number(process.env.STABILITY ?? 0.5),
      similarity_boost: 0.85,
      style: 0.4,
      use_speaker_boost: true,
    },
  };
  const res = await fetch(url, {
    method: "POST",
    headers: { "xi-api-key": apiKey, "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  if (!res.ok) throw new Error(`TTS ${res.status}: ${await res.text()}`);
  const json = await res.json();
  const mp3 = Buffer.from(json.audio_base64, "base64");
  const mp3Path = join(HERE, `${outBase}.mp3`);
  const wavPath = join(HERE, `${outBase}.wav`);
  const jsonPath = join(HERE, `${outBase}.json`);
  writeFileSync(mp3Path, mp3);
  execFileSync(
    "ffmpeg",
    ["-y", "-i", mp3Path, "-ac", "2", "-ar", "48000", "-c:a", "pcm_s16le", wavPath],
    { stdio: ["ignore", "ignore", "ignore"] },
  );
  writeFileSync(
    jsonPath,
    JSON.stringify(
      { text, alignment: json.alignment, normalized_alignment: json.normalized_alignment },
      null,
      2,
    ),
  );
  // crude duration from the last char's end time
  const al = json.alignment;
  const dur = al?.character_end_times_seconds?.at(-1) ?? null;
  return { wavPath, dur };
}

const SCRIPT = process.argv[2] || "vo-script.txt";
const OUTBASE = process.argv[3] || "vo";
const text = readFileSync(join(HERE, SCRIPT), "utf8").trim();
const apiKey = loadKey();
console.log(`[gen-vo] voice=${VOICE_ID} model=${MODEL_ID} stability=${process.env.STABILITY ?? 0.5} → ${OUTBASE}`);
const { dur } = await tts(apiKey, text, OUTBASE);
console.log(`[gen-vo] wrote ${OUTBASE}.mp3 / ${OUTBASE}.wav / ${OUTBASE}.json`);
console.log(`[gen-vo] duration ≈ ${dur ? dur.toFixed(1) + "s" : "unknown"}`);
