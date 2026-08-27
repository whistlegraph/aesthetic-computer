#!/usr/bin/env node
// dub-reel.mjs — translate a laerklokken reel with the ElevenLabs Dubbing API
// (voice-preserving) and scribe the result for karaoke subs. Produces the
// workdir reel-chrome.mjs expects.
//
// Usage:
//   node marketing/klokkentales/bin/dub-reel.mjs \
//     --in ~/Downloads/reel.mp4 --slug kosmisk-buffet \
//     [--source-lang da] [--target-lang en]
//
// Outputs (in out/reels/<slug>/): reel.mp4, reel-en-dub.mp4, dub-audio.mp3,
// subs-en.srt, words-en.json

import { copyFileSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { basename, dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { loadKlokkentalesEnv } from "../lib/env.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const env = loadKlokkentalesEnv();
const apiBase = env.ELEVENLABS_API_BASE || "https://api.elevenlabs.io";
if (!env.ELEVENLABS_API_KEY) throw new Error("ELEVENLABS_API_KEY missing");

const argv = process.argv.slice(2);
const flags = {};
for (let i = 0; i < argv.length; i++) {
  if (argv[i].startsWith("--")) { flags[argv[i].slice(2)] = argv[i + 1]; i++; }
}
const INPUT = resolve(flags.in || "");
const SLUG = flags.slug || basename(INPUT).replace(/\.[^.]+$/, "").toLowerCase().replace(/[^a-z0-9]+/g, "-");
const SOURCE = flags["source-lang"] || "da";
const TARGET = flags["target-lang"] || "en";
const DIR = resolve(HERE, "..", "out", "reels", SLUG);
mkdirSync(DIR, { recursive: true });

async function eleven(path, init = {}) {
  const response = await fetch(`${apiBase}${path}`, {
    ...init,
    headers: { "xi-api-key": env.ELEVENLABS_API_KEY, ...(init.headers || {}) },
    signal: AbortSignal.timeout(300_000),
  });
  if (!response.ok) throw new Error(`ElevenLabs ${response.status}: ${(await response.text()).slice(0, 500)}`);
  return response;
}

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

copyFileSync(INPUT, `${DIR}/reel.mp4`);
console.log(`workdir ${DIR}`);

// 1. dub (voice-preserving translation)
const form = new FormData();
form.append("file", new Blob([readFileSync(INPUT)], { type: "video/mp4" }), "reel.mp4");
form.append("source_lang", SOURCE);
form.append("target_lang", TARGET);
form.append("num_speakers", "0");
form.append("watermark", "false");
const { dubbing_id } = await (await eleven("/v1/dubbing", { method: "POST", body: form })).json();
console.log(`dubbing_id ${dubbing_id}`);

for (;;) {
  await sleep(5000);
  const meta = await (await eleven(`/v1/dubbing/${dubbing_id}`)).json();
  console.log(`  status: ${meta.status}`);
  if (meta.status === "dubbed") break;
  if (meta.status === "failed") throw new Error(`dub failed: ${meta.error || "unknown"}`);
}

const dubbed = Buffer.from(await (await eleven(`/v1/dubbing/${dubbing_id}/audio/${TARGET}`)).arrayBuffer());
writeFileSync(`${DIR}/reel-${TARGET}-dub.mp4`, dubbed);
const srt = await (await eleven(`/v1/dubbing/${dubbing_id}/transcript/${TARGET}?format_type=srt`)).text();
writeFileSync(`${DIR}/subs-${TARGET}.srt`, srt);

// 2. extract dub audio for scribe + the chrome mix
execFileSync("ffmpeg", ["-y", "-i", `${DIR}/reel-${TARGET}-dub.mp4`, "-vn",
  "-c:a", "libmp3lame", "-q:a", "2", `${DIR}/dub-audio.mp3`], { stdio: "inherit" });

// 3. scribe word timestamps for karaoke subs
const stt = new FormData();
stt.append("file", new Blob([readFileSync(`${DIR}/dub-audio.mp3`)], { type: "audio/mpeg" }), "dub-audio.mp3");
stt.append("model_id", "scribe_v1");
stt.append("language_code", TARGET);
const words = await (await eleven("/v1/speech-to-text", { method: "POST", body: stt })).json();
writeFileSync(`${DIR}/words-${TARGET}.json`, JSON.stringify(words, null, 2));

console.log(`\ntranscript: ${words.text}`);
console.log(`\ndone — next: node marketing/klokkentales/bin/reel-chrome.mjs --dir ${DIR} --music pop/marimba/out/marimbaba.mp3 --name ${SLUG}`);
