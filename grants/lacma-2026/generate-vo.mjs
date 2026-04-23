#!/usr/bin/env node
// generate-vo.mjs
//
// Generate LACMA grant voiceover via ElevenLabs TTS.
//
// Usage:
//   node generate-vo.mjs                           # uses default script (inline below) and writes video-vo.mp3/.wav
//   node generate-vo.mjs path/to/script.txt        # reads script from file
//   echo "hello" | node generate-vo.mjs -          # reads script from stdin
//   node generate-vo.mjs --clone sample.mp3        # create a Jeffrey voice clone (requires paid plan), prints voice_id
//
// Env overrides (optional):
//   VOICE_ID=xxx        // ElevenLabs voice id (default: Chris - premade stand-in)
//   MODEL_ID=xxx        // default: eleven_multilingual_v2
//   OUT_BASENAME=video-vo (written next to this script; .mp3 + .wav produced)
//
// Auth: reads ELEVENLABS_API_KEY from the AC vault env file
// (/Users/jas/aesthetic-computer/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env).
// No npm deps required; uses built-in fetch and child_process.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { resolve, dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { execFileSync } from "node:child_process";

const __filename = fileURLToPath(import.meta.url);
const __dirname = dirname(__filename);

const VAULT_ENV =
  "/Users/jas/aesthetic-computer/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env";

// Chris - Charming, Down-to-Earth | middle-aged American male — best available
// stand-in for Jeffrey until the ElevenLabs plan is upgraded and a proper voice
// clone is created.
const DEFAULT_VOICE_ID = "iP95p4xoKVk53GoZ742B";
// eleven_v3 is the newest, most expressive model (varies emotion across phrases).
// eleven_multilingual_v2 is a safe fallback if v3 chokes on a specific voice.
const DEFAULT_MODEL_ID = "eleven_v3";

// Inline 2-minute LACMA narration script.
const DEFAULT_SCRIPT = `Personal computers have not been very personal. Windows 10 end-of-life just stranded hundreds of millions of laptops. There has never been a better time to develop new software for old hardware.

This is AC Native: a bare-metal creative computing operating system. No desktop, no app store. Fifty dollars per seat. For the LACMA Art and Technology Lab, we propose growing it into a public device library — a lending fleet of AC Blank laptops flashed with our OS, circulating through Flash Days, workshops, and Family Play afternoons at LACMA. A public waitlist. The library welcomes artists flashing their own custom creative operating systems, joining a tradition of artist-run device libraries.

Aesthetic Computer has been under active development since 2021 — over nineteen thousand commits, seventeen thousand KidLisp programs, twenty-eight hundred registered handles. People draw, chat, and compose pieces together in real time. Every piece is URL-addressable, QR-shareable, and free to remix.

At the 2027 Symposium we boot the cohort. At the 2028 Demo Day we play the room. Flash days, workshops, and family play at LACMA.

Aesthetic dot Computer. A civic instrument. The new scene has just begun.`;

function loadApiKey() {
  if (process.env.ELEVENLABS_API_KEY) return process.env.ELEVENLABS_API_KEY;
  if (!existsSync(VAULT_ENV)) {
    throw new Error(
      `Vault env not found at ${VAULT_ENV}; set ELEVENLABS_API_KEY manually.`,
    );
  }
  const text = readFileSync(VAULT_ENV, "utf8");
  const line = text
    .split("\n")
    .find((l) => l.trim().startsWith("ELEVENLABS_API_KEY="));
  if (!line) throw new Error("ELEVENLABS_API_KEY not in vault env file.");
  return line.split("=", 2)[1].trim();
}

async function readStdin() {
  const chunks = [];
  for await (const c of process.stdin) chunks.push(c);
  return Buffer.concat(chunks).toString("utf8");
}

async function loadScript() {
  const arg = process.argv[2];
  if (!arg) return DEFAULT_SCRIPT;
  if (arg === "-") return (await readStdin()).trim();
  const path = resolve(process.cwd(), arg);
  return readFileSync(path, "utf8").trim();
}

async function tts({ text, voiceId, modelId, apiKey }) {
  const url = `https://api.elevenlabs.io/v1/text-to-speech/${voiceId}?output_format=mp3_44100_128`;
  // Lower stability (default 0.35) makes the delivery vary emotion with the
  // phrasing instead of reading monotone. Style (default 0.5) transfers
  // speaker style from the reference sample. Override via env:
  //   STABILITY=0.25 SIMILARITY=0.85 STYLE=0.6 node generate-vo.mjs
  const stability = Number(process.env.STABILITY ?? 0.35);
  const similarity = Number(process.env.SIMILARITY ?? 0.85);
  const style = Number(process.env.STYLE ?? 0.5);
  const body = {
    text,
    model_id: modelId,
    voice_settings: {
      stability,
      similarity_boost: similarity,
      style,
      use_speaker_boost: true,
    },
  };
  const res = await fetch(url, {
    method: "POST",
    headers: {
      "xi-api-key": apiKey,
      "Content-Type": "application/json",
      Accept: "audio/mpeg",
    },
    body: JSON.stringify(body),
  });
  if (!res.ok) {
    const errText = await res.text().catch(() => "");
    throw new Error(`ElevenLabs TTS failed (${res.status}): ${errText}`);
  }
  const buf = Buffer.from(await res.arrayBuffer());
  return buf;
}

function mp3ToWav(mp3Path, wavPath) {
  // 48 kHz stereo WAV, as requested.
  execFileSync(
    "ffmpeg",
    [
      "-y",
      "-i",
      mp3Path,
      "-ac",
      "2",
      "-ar",
      "48000",
      "-c:a",
      "pcm_s16le",
      wavPath,
    ],
    { stdio: ["ignore", "ignore", "inherit"] },
  );
}

async function cloneVoice({ samplePath, apiKey, name = "Jeffrey" }) {
  const { openAsBlob } = await import("node:fs");
  const form = new FormData();
  form.append("name", name);
  form.append(
    "description",
    "Jeffrey Alan Scudder, creator of Aesthetic Computer (LACMA 2026 grant narration)",
  );
  const blob = await openAsBlob(samplePath);
  form.append("files", blob, samplePath.split("/").pop());
  const res = await fetch("https://api.elevenlabs.io/v1/voices/add", {
    method: "POST",
    headers: { "xi-api-key": apiKey, Accept: "application/json" },
    body: form,
  });
  const json = await res.json().catch(() => ({}));
  if (!res.ok) {
    throw new Error(
      `Voice clone failed (${res.status}): ${JSON.stringify(json)}`,
    );
  }
  return json.voice_id;
}

async function main() {
  const apiKey = loadApiKey();

  // --clone subcommand: create a voice clone, print voice_id, exit.
  if (process.argv[2] === "--clone") {
    const samplePath = process.argv[3];
    if (!samplePath) {
      console.error("usage: node generate-vo.mjs --clone path/to/sample.mp3");
      process.exit(2);
    }
    const voiceId = await cloneVoice({
      samplePath: resolve(process.cwd(), samplePath),
      apiKey,
      name: process.env.CLONE_NAME || "Jeffrey",
    });
    console.error(
      `[generate-vo] cloned voice. set VOICE_ID=${voiceId} and re-run to synthesize.`,
    );
    console.log(voiceId);
    return;
  }

  const voiceId = process.env.VOICE_ID || DEFAULT_VOICE_ID;
  const modelId = process.env.MODEL_ID || DEFAULT_MODEL_ID;
  const baseName = process.env.OUT_BASENAME || "video-vo";
  const text = await loadScript();

  console.error(
    `[generate-vo] voice=${voiceId} model=${modelId} chars=${text.length}`,
  );

  const mp3Buf = await tts({ text, voiceId, modelId, apiKey });
  const mp3Path = join(__dirname, `${baseName}.mp3`);
  const wavPath = join(__dirname, `${baseName}.wav`);
  writeFileSync(mp3Path, mp3Buf);
  console.error(`[generate-vo] wrote ${mp3Path} (${mp3Buf.length} bytes)`);
  mp3ToWav(mp3Path, wavPath);
  console.error(`[generate-vo] wrote ${wavPath} (48 kHz stereo)`);
  console.log(wavPath);
}

main().catch((err) => {
  console.error(err.stack || err.message);
  process.exit(1);
});
