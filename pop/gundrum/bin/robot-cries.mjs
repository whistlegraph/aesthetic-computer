#!/usr/bin/env node
// robot-cries.mjs — the robot cries for gunrunmicro, made by ElevenLabs, into assets/snacks/.
//
// Two kinds, both written beside the chips and drinks (gitignored; CRIES.json records how each was made):
//   cry-NN.mp3    text to sound effect (eleven_text_to_sound_v2): one short cry per prompt below
//   mouth-NN.mp3  speech to speech (eleven_multilingual_sts_v2): a recorded crunch handed to a voice, which
//                 says it back as a mouth would
// A file that exists is kept, so a re-run only makes what is missing. Needs ELEVENLABS_API_KEY in the
// environment or the vault (lith/.env), and bin/fetch-snacks.mjs run first for the crunches.
//
//   node pop/gundrum/bin/robot-cries.mjs
import { existsSync, readFileSync, writeFileSync } from "node:fs";
import { homedir } from "node:os";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const OUT = resolve(dirname(fileURLToPath(import.meta.url)), "../assets/snacks");
const API = "https://api.elevenlabs.io/v1";

function key() {
  if (process.env.ELEVENLABS_API_KEY) return process.env.ELEVENLABS_API_KEY;
  const env = resolve(homedir(), "aesthetic-computer-vault/lith/.env");
  const line = existsSync(env) && readFileSync(env, "utf8").split("\n").find((l) => l.startsWith("ELEVENLABS_API_KEY="));
  if (!line) throw new Error("no ELEVENLABS_API_KEY (environment or vault lith/.env)");
  return line.slice(line.indexOf("=") + 1).trim().replace(/^['"]|['"]$/g, "");
}

const CRIES = [
  "a tiny cute robot crying, one short sad whimper, dry and close, no music",
  "a small robot sobbing, two quick glitchy sobs, dry and close",
  "a little android wailing, its voice falling in pitch and breaking up, dry",
  "a toy robot bursting into tears, a high squeaky cry, dry and close",
  "a baby robot crying 'waah' through a cheap speaker, short, dry",
  "a sad robot hiccuping while it cries, bleepy and wet, short, dry",
  "a small robot whimpering with a stuttering digital voice, short, dry",
  "a cute robot crying with its mouth full, crunchy and garbled, short, dry",
  "a tiny robot sniffling and letting out one squeaky sob, dry and close",
  "a broken toy robot crying, pitch wobbling and bit-crushed, short, dry",
  "a little robot yelping in surprise, then a short cry, dry and close",
  "a small robot crying into a tin can, metallic and nasal, short, dry",
];
// crunches handed to a voice: [file in assets/snacks, voice id, voice name]
const MOUTHS = [
  ["crunch-chew-01.mp3", "cgSgspJ2msm6clMCkdW9", "Jessica"],
  ["crunch-bite-01.mp3", "FGY2WhTYpPnrIDTdsKH5", "Laura"],
  ["crunch-apple-01.mp3", "N2lVS1w4EtoT3dr4eOWO", "Callum"],
  ["crunch-tortilla-01.mp3", "pFZP5JQG7iQjIQuC4Bku", "Lily"],
];

const K = key(), made = [];
const two = (n) => String(n + 1).padStart(2, "0");
async function post(url, init, dest) {
  const res = await fetch(url, init);
  if (!res.ok) throw new Error(`${res.status} ${(await res.text()).slice(0, 200)}`);
  writeFileSync(dest, Buffer.from(await res.arrayBuffer()));
}
for (const [i, text] of CRIES.entries()) {
  const file = `cry-${two(i)}.mp3`, dest = resolve(OUT, file);
  try {
    if (!existsSync(dest)) await post(`${API}/sound-generation?output_format=mp3_44100_128`, { method: "POST", headers: { "xi-api-key": K, "Content-Type": "application/json" },
      body: JSON.stringify({ text, duration_seconds: 1.5, prompt_influence: .55, model_id: "eleven_text_to_sound_v2" }) }, dest);
    made.push({ file, kind: "text to sound effect", model: "eleven_text_to_sound_v2", prompt: text });
    console.log(`✓ ${file}  ${text}`);
  } catch (e) { console.error(`✗ ${file}: ${e.message}`); process.exitCode = 1; }
}
for (const [i, [src, voice, name]] of MOUTHS.entries()) {
  const file = `mouth-${two(i)}.mp3`, dest = resolve(OUT, file);
  try {
    if (!existsSync(dest)) {
      const form = new FormData();
      form.set("model_id", "eleven_multilingual_sts_v2");
      form.set("audio", new Blob([readFileSync(resolve(OUT, src))], { type: "audio/mpeg" }), src);
      await post(`${API}/speech-to-speech/${voice}?output_format=mp3_44100_128`, { method: "POST", headers: { "xi-api-key": K }, body: form }, dest);
    }
    made.push({ file, kind: "speech to speech", model: "eleven_multilingual_sts_v2", from: src, voice: name });
    console.log(`✓ ${file}  ${src} said back by ${name}`);
  } catch (e) { console.error(`✗ ${file}: ${e.message}`); process.exitCode = 1; }
}
writeFileSync(resolve(OUT, "CRIES.json"), JSON.stringify({ note: "Generated with ElevenLabs by bin/robot-cries.mjs. The mouth files are speech-to-speech renderings of the CC0 crunches in SOURCES.json.", sounds: made }, null, 2) + "\n");
