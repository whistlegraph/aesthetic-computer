#!/usr/bin/env node
// generate-vo-timed.mjs — regenerate hook + main VO via ElevenLabs
// with-timestamps endpoint so we can build precise SRT from alignment.
//
// Usage:  node generate-vo-timed.mjs
// Writes: vo-hook.wav + vo-hook.json + vo-main.wav + vo-main.json

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = dirname(fileURLToPath(import.meta.url));
const VAULT_ENV = "/Users/jas/aesthetic-computer/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env";

function loadKey() {
  if (process.env.ELEVENLABS_API_KEY) return process.env.ELEVENLABS_API_KEY;
  return readFileSync(VAULT_ENV, "utf8")
    .split("\n").find(l => l.startsWith("ELEVENLABS_API_KEY="))
    .split("=", 2)[1].trim();
}

const HOOK = `Personal computers have not been very personal. Windows 10 end-of-life just stranded hundreds of millions of laptops. There has never been a better time to develop new software for old hardware.`;

const MAIN = `This is AC Native. A bare-metal creative computing operating system. No desktop, no app store. Fifty dollars per seat. For the LACMA Art and Technology Lab, we propose growing it into a public device library. A lending fleet of AC Blank laptops flashed with our OS, circulating through Flash Days, workshops, and Family Play afternoons at LACMA. A public waitlist. The library welcomes artists flashing their own custom creative operating systems, joining a tradition of artist-run device libraries. Aesthetic Computer has been under active development since 2021. Over nineteen thousand commits. Seventeen thousand KidLisp programs. Twenty-eight hundred registered handles. People draw, chat, and compose pieces together in real time. At the 2027 Symposium we boot the cohort. At the 2028 Demo Day we play the room. Aesthetic dot Computer. A civic instrument. The new scene has just begun.`;

const VOICE_ID = "iP95p4xoKVk53GoZ742B"; // Chris
const MODEL_ID = "eleven_v3";

async function tts(text, outBase) {
  const apiKey = loadKey();
  const url = `https://api.elevenlabs.io/v1/text-to-speech/${VOICE_ID}/with-timestamps?output_format=mp3_44100_128`;
  const body = {
    text, model_id: MODEL_ID,
    voice_settings: { stability: 0.3, similarity_boost: 0.8, style: 0.6, use_speaker_boost: true },
  };
  const res = await fetch(url, {
    method: "POST",
    headers: { "xi-api-key": apiKey, "Content-Type": "application/json" },
    body: JSON.stringify(body),
  });
  if (!res.ok) throw new Error(`ElevenLabs failed ${res.status}: ${await res.text()}`);
  const json = await res.json();
  const mp3 = Buffer.from(json.audio_base64, "base64");
  const mp3Path = join(__dirname, `${outBase}.mp3`);
  const wavPath = join(__dirname, `${outBase}.wav`);
  const jsonPath = join(__dirname, `${outBase}.json`);
  writeFileSync(mp3Path, mp3);
  execFileSync("ffmpeg", ["-y", "-i", mp3Path, "-ac", "2", "-ar", "48000", "-c:a", "pcm_s16le", wavPath],
    { stdio: ["ignore", "ignore", "ignore"] });
  writeFileSync(jsonPath, JSON.stringify({
    text,
    alignment: json.alignment,
    normalized_alignment: json.normalized_alignment,
  }, null, 2));
  console.error(`  wrote ${outBase}.{wav,json}  (${json.alignment.characters.length} chars aligned)`);
  return { text, alignment: json.alignment };
}

const hook = await tts(HOOK, "vo-hook");
const main = await tts(MAIN, "vo-main");

console.log(`hook end: ${hook.alignment.character_end_times_seconds.at(-1).toFixed(2)}s`);
console.log(`main end: ${main.alignment.character_end_times_seconds.at(-1).toFixed(2)}s`);
