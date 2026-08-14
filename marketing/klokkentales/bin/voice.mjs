#!/usr/bin/env node

import { createHash } from "node:crypto";
import { execFileSync } from "node:child_process";
import {
  existsSync,
  mkdirSync,
  readFileSync,
  readdirSync,
  renameSync,
  writeFileSync,
} from "node:fs";
import { basename, extname, resolve } from "node:path";
import { klokkentalesVault, loadKlokkentalesEnv } from "../lib/env.mjs";

const env = loadKlokkentalesEnv();
const apiBase = env.ELEVENLABS_API_BASE || "https://api.elevenlabs.io";
const voiceRoot = resolve(klokkentalesVault, "voices", "prutti");
const sourceRoot = resolve(voiceRoot, "source");
const receiptPath = resolve(voiceRoot, "voice.json");
const projectEnvPath = resolve(klokkentalesVault, ".env");
const command = process.argv[2] || "status";
const argv = process.argv.slice(3);
const audioExtensions = new Set([".mp3", ".wav", ".m4a", ".flac", ".ogg"]);

function duration(path) {
  return Number(execFileSync("ffprobe", [
    "-v", "error", "-show_entries", "format=duration", "-of", "csv=p=0", path,
  ]).toString().trim());
}

function sourceFiles() {
  if (!existsSync(sourceRoot)) return [];
  return readdirSync(sourceRoot)
    .filter((name) => audioExtensions.has(extname(name).toLowerCase()))
    .map((name) => resolve(sourceRoot, name))
    .sort();
}

function requireApiKey() {
  if (!env.ELEVENLABS_API_KEY) {
    throw new Error(`ELEVENLABS_API_KEY is missing from the environment and ${resolve(klokkentalesVault, "..", "lith", ".env")}`);
  }
  return env.ELEVENLABS_API_KEY;
}

async function eleven(path, init = {}) {
  const response = await fetch(`${apiBase}${path}`, {
    ...init,
    headers: { "xi-api-key": requireApiKey(), ...(init.headers || {}) },
    signal: AbortSignal.timeout(120_000),
  });
  if (!response.ok) throw new Error(`ElevenLabs ${response.status}: ${(await response.text()).slice(0, 500)}`);
  return response;
}

function writeProjectEnv(key, value) {
  mkdirSync(klokkentalesVault, { recursive: true });
  const lines = existsSync(projectEnvPath) ? readFileSync(projectEnvPath, "utf8").split(/\r?\n/) : [];
  const next = [];
  let replaced = false;
  for (const line of lines) {
    if (line.startsWith(`${key}=`)) {
      if (!replaced) next.push(`${key}=${value}`);
      replaced = true;
    } else if (line) next.push(line);
  }
  if (!replaced) next.push(`${key}=${value}`);
  const temporary = `${projectEnvPath}.new`;
  writeFileSync(temporary, `${next.join("\n")}\n`, { mode: 0o600 });
  renameSync(temporary, projectEnvPath);
}

async function status() {
  const files = sourceFiles();
  const seconds = files.reduce((sum, file) => sum + duration(file), 0);
  console.log(`ElevenLabs account: ${env.ELEVENLABS_API_KEY ? "ready" : "missing API key"}`);
  if (env.ELEVENLABS_API_KEY) {
    const response = await eleven("/v1/user/subscription");
    const subscription = await response.json();
    const slots = subscription.voice_slots_used == null
      ? ""
      : ` · ${subscription.voice_slots_used}/${subscription.voice_limit} voice slots`;
    console.log(`ElevenLabs plan: ${subscription.tier}${slots}`);
  }
  console.log(`Prutti source: ${files.length} file${files.length === 1 ? "" : "s"} · ${seconds.toFixed(1)}s · ${sourceRoot}`);
  console.log(`Prutti voice: ${env.PRUTTI_ELEVENLABS_VOICE_ID || "not created"}`);
  if (env.PRUTTI_ELEVENLABS_VOICE_ID && env.ELEVENLABS_API_KEY) {
    const response = await eleven(`/v1/voices/${encodeURIComponent(env.PRUTTI_ELEVENLABS_VOICE_ID)}`);
    const voice = await response.json();
    console.log(`ElevenLabs confirms: ${voice.name} · ${voice.category}`);
  }
}

function prepare() {
  const input = argv.find((arg) => !arg.startsWith("--"));
  const value = (flag, fallback) => {
    const index = argv.indexOf(`--${flag}`);
    return index >= 0 ? argv[index + 1] : fallback;
  };
  if (!input || !existsSync(resolve(input))) {
    throw new Error("usage: voice.mjs prepare <approved-audio> [--start seconds] [--duration seconds]");
  }
  const start = Number(value("start", 0));
  const requested = Number(value("duration", Math.min(120, duration(resolve(input)) - start)));
  if (!Number.isFinite(start) || !Number.isFinite(requested) || start < 0 || requested <= 0 || requested > 180) {
    throw new Error("start must be non-negative and duration must be 1–180 seconds");
  }
  mkdirSync(sourceRoot, { recursive: true });
  const output = resolve(sourceRoot, `${basename(input, extname(input))}-${start}-${requested}.mp3`);
  execFileSync("ffmpeg", [
    "-y", "-ss", String(start), "-t", String(requested), "-i", resolve(input),
    "-vn", "-af", "highpass=f=55,lowpass=f=12000,loudnorm=I=-20:LRA=8:TP=-2",
    "-ar", "44100", "-ac", "1", "-b:a", "192k", output,
  ], { stdio: "ignore" });
  console.log(`prepared ${duration(output).toFixed(1)}s -> ${output}`);
  console.log("Listen to this complete file and remove any other speakers, music, private speech, or unapproved material before create-ivc.");
}

async function createIvc() {
  if (!argv.includes("--confirm-rights-and-consent")) {
    throw new Error("create-ivc requires --confirm-rights-and-consent, confirming Prutti approved this voice clone and every supplied recording is authorized for this use");
  }
  if (env.PRUTTI_ELEVENLABS_VOICE_ID) {
    throw new Error("PRUTTI_ELEVENLABS_VOICE_ID already exists; refusing to create a duplicate voice");
  }
  const files = sourceFiles();
  if (!files.length) throw new Error(`no approved audio in ${sourceRoot}; run voice.mjs prepare first`);
  const samples = files.map((file) => ({
    file,
    seconds: duration(file),
    sha256: createHash("sha256").update(readFileSync(file)).digest("hex"),
  }));
  const total = samples.reduce((sum, sample) => sum + sample.seconds, 0);
  if (total < 60 || total > 180) {
    throw new Error(`IVC source totals ${total.toFixed(1)}s; supply 60–180 seconds of clean, single-speaker audio`);
  }

  const form = new FormData();
  form.append("name", "Prutti · Klokkentales IVC");
  form.append("description", "Prutti storybook voice for the consented Klokkentales production on Aesthetic Computer.");
  form.append("remove_background_noise", "false");
  form.append("labels", JSON.stringify({ language: "da", use: "klokkentales" }));
  for (const sample of samples) {
    form.append("files", new Blob([readFileSync(sample.file)], { type: "audio/mpeg" }), basename(sample.file));
  }
  const response = await eleven("/v1/voices/add", { method: "POST", body: form });
  const result = await response.json();
  if (!result.voice_id) throw new Error("ElevenLabs response did not contain voice_id");

  mkdirSync(voiceRoot, { recursive: true });
  writeFileSync(receiptPath, JSON.stringify({
    voiceId: result.voice_id,
    type: "instant-voice-clone",
    createdAt: new Date().toISOString(),
    requiresVerification: Boolean(result.requires_verification),
    sources: samples.map(({ file, seconds, sha256 }) => ({ name: basename(file), seconds, sha256 })),
  }, null, 2) + "\n", { mode: 0o600 });
  writeProjectEnv("PRUTTI_ELEVENLABS_VOICE_ID", result.voice_id);
  console.log(`created Prutti IVC ${result.voice_id}`);
  console.log(`private receipt: ${receiptPath}`);
  console.log("Next: node bin/voice.mjs sample");
}

async function sample() {
  const voiceId = env.PRUTTI_ELEVENLABS_VOICE_ID;
  if (!voiceId) throw new Error("Prutti voice has not been created");
  const text = "The clock is still running. Good. Then we are not late.";
  const response = await eleven(`/v1/text-to-speech/${encodeURIComponent(voiceId)}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      text,
      model_id: "eleven_multilingual_v2",
      voice_settings: { stability: 0.38, similarity_boost: 0.9, style: 0.48, use_speaker_boost: true, speed: 0.98 },
    }),
  });
  const output = resolve(voiceRoot, "review.mp3");
  writeFileSync(output, Buffer.from(await response.arrayBuffer()), { mode: 0o600 });
  console.log(`review the complete voice sample: ${output}`);
}

// Speak an arbitrary line in Prutti's voice. Same settings as `sample`, but
// the text and destination are yours:
//   node bin/voice.mjs say "aesthetic dot computer" --out ~/stamp.mp3
async function say() {
  // The receipt is the source of truth for the voice id; the project .env is
  // only a convenience pointer and may be absent on a fresh checkout.
  const voiceId = env.PRUTTI_ELEVENLABS_VOICE_ID || (existsSync(receiptPath)
    ? JSON.parse(readFileSync(receiptPath, "utf8")).voiceId
    : null);
  if (!voiceId) throw new Error("Prutti voice has not been created");
  const text = argv.find((arg) => !arg.startsWith("--"));
  if (!text) throw new Error('usage: voice.mjs say "the line" [--out path.mp3]');
  const outFlag = argv.indexOf("--out");
  const output = outFlag >= 0 && argv[outFlag + 1]
    ? resolve(argv[outFlag + 1])
    : resolve(voiceRoot, "say.mp3");
  const response = await eleven(`/v1/text-to-speech/${encodeURIComponent(voiceId)}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      text,
      model_id: "eleven_multilingual_v2",
      voice_settings: { stability: 0.38, similarity_boost: 0.9, style: 0.48, use_speaker_boost: true, speed: 0.98 },
    }),
  });
  mkdirSync(resolve(output, ".."), { recursive: true });
  writeFileSync(output, Buffer.from(await response.arrayBuffer()), { mode: 0o600 });
  console.log(`${output}`);
}

if (command === "status") await status();
else if (command === "prepare") prepare();
else if (command === "create-ivc") await createIvc();
else if (command === "sample") await sample();
else if (command === "say") await say();
else throw new Error("usage: voice.mjs status | prepare <audio> | create-ivc --confirm-rights-and-consent | sample");
