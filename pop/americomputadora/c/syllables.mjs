#!/usr/bin/env node
// syllables.mjs — fetch the jeffrey syllable chant via /api/say (ElevenLabs
// jeffrey-pvc proxy, same provider as pop/bin/say.mjs) and trim to tight
// per-syllable wavs. cached by file existence — each syllable costs once.
//
//   'ah' 'mer' 'ick' 'ahmp' 'uter' 'dora' — one per beat in the verses,
//   pitched (by bake.mjs) to follow the toy-piano melody tones.
//
// usage: node c/syllables.mjs [--force]

import { writeFileSync, mkdirSync, existsSync, rmSync } from "node:fs";
import { dirname, join } from "node:path";
import { fileURLToPath } from "node:url";
import { spawnSync } from "node:child_process";

const HERE = dirname(fileURLToPath(import.meta.url));
const SR = 48_000;
const cacheDir = join(HERE, "syl-cache");
mkdirSync(cacheDir, { recursive: true });

const force = process.argv.includes("--force");
const SAY_URL = process.env.SAY_ENDPOINT || "https://aesthetic.computer/api/say";

// name → what jeffrey actually says (text shapes the pronunciation).
// the literal phonetics of americomputadora: a-me-ri-com-pu-ta-do-ra.
// eight syllables — grouped [4,2,2] (americom / puta / dora), and an even
// 8 against a 4-beat bar so the word restarts on the downbeat every 2 bars
// instead of precessing (the old 7-syllable pun drifted out of phase).
const SYLS = [
  ["a",   "ah"],
  ["me",  "meh"],
  ["ri",  "ree"],
  ["com", "comb"],
  ["pu",  "poo"],
  ["ta",  "tah"],
  ["do",  "doe"],
  ["ra",  "rah"],
];

// a second voice — a macOS "computer" — chants the SAME syllables in
// parallel (c-<name>.wav). cheap, local, no API.
const COMPUTER_VOICE = "Samantha";
for (const [name, text] of SYLS) {
  const cwav = join(cacheDir, `c-${name}.wav`);
  if (!existsSync(cwav) || force) {
    const aiff = cwav.replace(/\.wav$/, ".aiff");
    spawnSync("say", ["-v", COMPUTER_VOICE, "-r", "140", "-o", aiff, text], { stdio: "ignore" });
    spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", "-i", aiff,
      "-af",
      "silenceremove=start_periods=1:start_threshold=-40dB:start_silence=0.01," +
      "areverse,silenceremove=start_periods=1:start_threshold=-40dB:start_silence=0.02,areverse," +
      "afade=t=in:d=0.005,loudnorm=I=-15:TP=-1.2",
      "-ac", "1", "-ar", String(SR), cwav], { stdio: "ignore" });
    rmSync(aiff, { force: true });
    console.log(`✓ c-${name} (${COMPUTER_VOICE})`);
  }
}

for (const [name, text] of SYLS) {
  const mp3 = join(cacheDir, `${name}.mp3`);
  const wav = join(cacheDir, `${name}.wav`);
  if (!existsSync(mp3) || force) {
    console.log(`→ /api/say jeffrey: "${text}"`);
    const res = await fetch(SAY_URL, {
      method: "POST",
      headers: { "content-type": "application/json" },
      body: JSON.stringify({ from: text, provider: "jeffrey", voice: "neutral:0" }),
    }).catch(() => null);
    if (res && res.ok) {
      writeFileSync(mp3, Buffer.from(await res.arrayBuffer()));
    } else if (process.env.ELEVENLABS_API_KEY) {
      // proxy down but a key is in the env — go straight to ElevenLabs
      // with the jeffrey-pvc voice (same id as system/netlify/functions/say.js).
      console.warn(`  ! /api/say down — direct ElevenLabs (jeffrey-pvc)`);
      const el = await fetch("https://api.elevenlabs.io/v1/text-to-speech/ZXoQQp5X0PKHGwyZpVIT", {
        method: "POST",
        headers: {
          "xi-api-key": process.env.ELEVENLABS_API_KEY,
          "content-type": "application/json",
        },
        body: JSON.stringify({
          text,
          model_id: "eleven_multilingual_v2",
          voice_settings: { stability: 0.5, similarity_boost: 0.75 },
        }),
      });
      if (!el.ok) { console.error(`✗ ElevenLabs ${el.status}: ${await el.text()}`); process.exit(1); }
      writeFileSync(mp3, Buffer.from(await el.arrayBuffer()));
    } else {
      // PLACEHOLDER: /api/say's ElevenLabs key is currently invalid (401
      // upstream). macOS Reed stands in for jeffrey until the lith key is
      // fixed — then: node c/syllables.mjs --force
      if (res) console.warn(`  ! /api/say ${res.status} — using macOS placeholder voice`);
      const aiff = mp3.replace(/\.mp3$/, ".aiff");
      const ok = spawnSync("say", ["-v", "Reed (English (US))", "-r", "150", "-o", aiff, text],
        { stdio: ["ignore", "ignore", "inherit"] }).status === 0;
      if (!ok) { console.error("✗ say fallback failed"); process.exit(1); }
      spawnSync("ffmpeg", ["-hide_banner", "-y", "-loglevel", "error", "-i", aiff,
        "-c:a", "libmp3lame", "-q:a", "2", mp3], { stdio: "ignore" });
      rmSync(aiff, { force: true });
    }
  }
  if (!existsSync(wav) || force) {
    rmSync(wav, { force: true });
    const ok = spawnSync("ffmpeg", [
      "-hide_banner", "-y", "-loglevel", "error", "-i", mp3,
      "-af",
      "silenceremove=start_periods=1:start_threshold=-40dB:start_silence=0.01," +
      "areverse,silenceremove=start_periods=1:start_threshold=-40dB:start_silence=0.02,areverse," +
      "afade=t=in:d=0.005,loudnorm=I=-15:TP=-1.2",
      "-ac", "1", "-ar", String(SR), wav,
    ], { stdio: ["ignore", "ignore", "inherit"] }).status === 0;
    console.log(`${ok ? "✓" : "✗"} ${wav}`);
    if (!ok) process.exit(1);
  } else {
    console.log(`✓ ${name} cached`);
  }
}
console.log("# all syllables ready — run node c/bake.mjs to pitch + table them");
