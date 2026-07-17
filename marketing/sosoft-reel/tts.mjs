#!/usr/bin/env node
import { createHash } from "node:crypto";
import { existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";

const ROOT = dirname(fileURLToPath(import.meta.url));
const OUT = resolve(ROOT, "out");
const text = readFileSync(resolve(ROOT, "narration.txt"), "utf8").trim();
const force = process.argv.includes("--force");
const body = { from: text, provider: "jeffrey", voice: "neutral:0" };
const hash = createHash("sha256").update(JSON.stringify(body)).digest("hex").slice(0, 16);
const audio = resolve(OUT, "narration.mp3");
const hashPath = `${audio}.hash`;
mkdirSync(OUT, { recursive: true });

if (!force && existsSync(audio) && existsSync(hashPath) && readFileSync(hashPath, "utf8").trim() === hash) {
  console.log(`cached ${audio} · ${hash}`);
  process.exit(0);
}

console.log(`requesting Jeffrey PVC · ${text.split(/\s+/).length} words`);
let response = await fetch("https://aesthetic.computer/api/say", {
  method: "POST",
  headers: { "Content-Type": "application/json" },
  body: JSON.stringify(body),
  redirect: "follow",
}).catch(() => null);

let bytes;
if (response?.ok) {
  bytes = Buffer.from(await response.arrayBuffer());
} else {
  // Production occasionally sits behind a transient Cloudflare 522. The local
  // vault carries the same ElevenLabs credential used by lith; fall back to the
  // timestamp endpoint so a production render is not coupled to origin health.
  const vaultPath = resolve(ROOT, "..", "..", "aesthetic-computer-vault", "lith", ".env");
  if (!existsSync(vaultPath)) throw new Error(`/api/say unavailable and ${vaultPath} is missing`);
  const env = Object.fromEntries(readFileSync(vaultPath, "utf8").split(/\r?\n/).map((line) => {
    const i = line.indexOf("=");
    return i > 0 ? [line.slice(0, i), line.slice(i + 1).replace(/^['\"]|['\"]$/g, "")] : [];
  }).filter((pair) => pair.length === 2));
  if (!env.ELEVENLABS_API_KEY) throw new Error("ELEVENLABS_API_KEY missing from lith vault");
  console.log(`origin unavailable (${response?.status || "network"}); using direct timestamp endpoint`);
  response = await fetch("https://api.elevenlabs.io/v1/text-to-speech/ZXoQQp5X0PKHGwyZpVIT/with-timestamps", {
    method: "POST",
    headers: { "Content-Type": "application/json", "xi-api-key": env.ELEVENLABS_API_KEY },
    body: JSON.stringify({
      text,
      model_id: "eleven_multilingual_v2",
      voice_settings: { stability: 0.65, similarity_boost: 0.9, style: 0.15, use_speaker_boost: true, speed: 1.0 },
    }),
  });
  if (!response.ok) throw new Error(`ElevenLabs ${response.status}: ${await response.text()}`);
  const result = await response.json();
  bytes = Buffer.from(result.audio_base64, "base64");
  writeFileSync(resolve(OUT, "narration-alignment.json"), JSON.stringify({
    alignment: result.alignment,
    normalizedAlignment: result.normalized_alignment,
  }, null, 2) + "\n");
}
writeFileSync(audio, bytes);
writeFileSync(hashPath, `${hash}\n`);
console.log(`wrote ${audio} · ${(bytes.length / 1024).toFixed(0)} KB · ${hash}`);
