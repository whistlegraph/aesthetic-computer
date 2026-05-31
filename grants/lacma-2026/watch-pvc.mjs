#!/usr/bin/env node
// watch-pvc.mjs — poll the ElevenLabs PVC voice until fine-tuning
// completes on at least one model, then auto-regenerate the VO in
// Jeffrey's cloned voice, rebuild the video (v4), and copy to Desktop.
//
// Run: node watch-pvc.mjs
// Stops itself on success; Ctrl-C to abort.

import { readFileSync, writeFileSync, existsSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { join, dirname } from "node:path";
import { fileURLToPath } from "node:url";

const HERE = dirname(fileURLToPath(import.meta.url));
const VOICE_ID = "dYNGZ848Oo6DtNBoeqgh"; // @jeffrey PVC
const VAULT_ENV = "/Users/jas/aesthetic-computer/aesthetic-computer-vault/.devcontainer/envs/devcontainer.env";

function loadKey() {
  if (process.env.ELEVENLABS_API_KEY) return process.env.ELEVENLABS_API_KEY;
  return readFileSync(VAULT_ENV, "utf8")
    .split("\n").find(l => l.startsWith("ELEVENLABS_API_KEY="))
    .split("=", 2)[1].trim();
}

// Prefer higher-quality fine-tuned models when available.
const MODEL_PREFERENCE = [
  "eleven_multilingual_v2",
  "eleven_turbo_v2_5",
  "eleven_turbo_v2",
  "eleven_flash_v2_5",
  "eleven_v2_5_flash",
  "eleven_flash_v2",
  "eleven_v2_flash",
];

async function status(apiKey) {
  const r = await fetch(`https://api.elevenlabs.io/v1/voices/${VOICE_ID}`, {
    headers: { "xi-api-key": apiKey },
  });
  if (!r.ok) throw new Error(`voice status ${r.status}: ${await r.text()}`);
  const d = await r.json();
  return d.fine_tuning?.state || {};
}

async function ttsWithTimestamps(apiKey, modelId, text, outBase) {
  const url = `https://api.elevenlabs.io/v1/text-to-speech/${VOICE_ID}/with-timestamps?output_format=mp3_44100_128`;
  const body = {
    text, model_id: modelId,
    voice_settings: { stability: 0.3, similarity_boost: 0.85, style: 0.5, use_speaker_boost: true },
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
  execFileSync("ffmpeg", ["-y", "-i", mp3Path, "-ac", "2", "-ar", "48000", "-c:a", "pcm_s16le", wavPath],
    { stdio: ["ignore", "ignore", "ignore"] });
  writeFileSync(jsonPath, JSON.stringify({ text, alignment: json.alignment, normalized_alignment: json.normalized_alignment }, null, 2));
  return { wavPath, jsonPath };
}

const HOOK = `Personal computers have not been very personal. Windows 10 end-of-life just stranded hundreds of millions of laptops. There has never been a better time to develop new software for old hardware.`;

const MAIN = `This is AC Native. A bare-metal creative computing operating system. No desktop, no app store. Fifty dollars per seat. For the LACMA Art and Technology Lab, we propose growing it into a public device library. A lending fleet of AC Blank laptops flashed with our OS, circulating through Flash Days, workshops, and Family Play afternoons at LACMA. A public waitlist. The library welcomes artists flashing their own custom creative operating systems, joining a tradition of artist-run device libraries. Aesthetic Computer has been under active development since 2021. Over nineteen thousand commits. Seventeen thousand KidLisp programs. Twenty-eight hundred registered handles. People draw, chat, and compose pieces together in real time. At the 2027 Symposium we boot the cohort. At the 2028 Demo Day we play the room. Aesthetic dot Computer. A civic instrument. The new scene has just begun.`;

function notify(title, msg) {
  try {
    execFileSync("osascript", [
      "-e",
      `display notification "${msg.replace(/"/g, '\\"')}" with title "${title.replace(/"/g, '\\"')}" sound name "Glass"`,
    ]);
  } catch {}
}

async function main() {
  const apiKey = loadKey();
  console.log(`[watch-pvc] voice_id=${VOICE_ID}`);
  console.log(`[watch-pvc] polling every 20s...`);
  let readyModel = null;
  const start = Date.now();
  while (!readyModel) {
    try {
      const s = await status(apiKey);
      const summary = Object.entries(s).map(([m, st]) => `${m.replace("eleven_", "")}=${st}`).join(" ");
      const elapsed = Math.round((Date.now() - start) / 60000);
      console.log(`[+${elapsed}m] ${summary}`);
      for (const m of MODEL_PREFERENCE) {
        if (s[m] === "fine_tuned") { readyModel = m; break; }
      }
    } catch (e) {
      console.error(`  poll error: ${e.message}`);
    }
    if (!readyModel) await new Promise(r => setTimeout(r, 20000));
  }

  console.log(`\n✓ PVC ready on model ${readyModel}! Regenerating VO...`);
  notify("LACMA PVC", `Jeffrey voice ready on ${readyModel} — regenerating VO...`);

  await ttsWithTimestamps(apiKey, readyModel, HOOK, "vo-hook-pvc");
  await ttsWithTimestamps(apiKey, readyModel, MAIN, "vo-main-pvc");
  console.log(`  wrote vo-hook-pvc.{wav,json}, vo-main-pvc.{wav,json}`);

  // Patch build-video-v3.py to use the PVC files, or run a dedicated v4 script.
  // Simpler: temporarily symlink the vo-*.wav files to point to PVC versions.
  // We'll generate v4 by copying v3 build into v4 with renamed inputs.
  console.log("→ running v4 build using PVC VO...");
  const v4Script = join(HERE, "build-video-v4.py");
  const v3 = readFileSync(join(HERE, "build-video-v3.py"), "utf8")
    .replaceAll("video-cards-v3", "video-cards-v4")
    .replaceAll("lacma-grant-video-v3.mp4", "lacma-grant-video-v4.mp4")
    .replaceAll('"vo-hook.wav"', '"vo-hook-pvc.wav"')
    .replaceAll('"vo-hook.json"', '"vo-hook-pvc.json"')
    .replaceAll('"vo-main.wav"', '"vo-main-pvc.wav"')
    .replaceAll('"vo-main.json"', '"vo-main-pvc.json"');
  writeFileSync(v4Script, v3);
  execFileSync("python3", [v4Script], { stdio: "inherit", cwd: HERE });

  // Copy to Desktop
  const finalVideo = join(HERE, "lacma-grant-video-v4.mp4");
  const desktopVideo = `${process.env.HOME}/Desktop/LACMA-GRANT-VIDEO.mp4`;
  execFileSync("cp", [finalVideo, desktopVideo]);
  console.log(`\n✓ wrote ${finalVideo} and copied to ${desktopVideo}`);
  notify("LACMA video ready", "v4 with Jeffrey PVC voice saved to Desktop. Upload to YouTube.");

  console.log("\ndone.");
}

main().catch(e => {
  console.error(e.stack || e.message);
  process.exit(1);
});
