#!/usr/bin/env node

// Pruttivox: read a community text aloud in the consented Prutti IVC.
// Every clip ends with a spoken synthetic-voice tag and is logged to the
// vault with its requester, so provenance survives the disposable out/ dir.
//
//   node bin/pruttivox.mjs "Hej klokken, god torsdag" --from @snakes
//   node bin/pruttivox.mjs "..." --slug god-torsdag --publish

import { appendFileSync, existsSync, mkdirSync, readFileSync, writeFileSync } from "node:fs";
import { execFileSync } from "node:child_process";
import { dirname, resolve } from "node:path";
import { fileURLToPath } from "node:url";
import { klokkentalesVault, loadKlokkentalesEnv } from "../lib/env.mjs";

const HERE = dirname(fileURLToPath(import.meta.url));
const outRoot = resolve(HERE, "..", "out", "pruttivox");
const logPath = resolve(klokkentalesVault, "pruttivox", "log.jsonl");
const tagPath = resolve(outRoot, "tag.mp3");
const tagText = "Pruttivox. Syntetisk stemme.";
const publishPrefix = "klokkentales/pruttivox";

const env = loadKlokkentalesEnv();
const apiBase = env.ELEVENLABS_API_BASE || "https://api.elevenlabs.io";
const receiptPath = resolve(klokkentalesVault, "voices", "prutti", "voice.json");
const voiceId = env.PRUTTI_ELEVENLABS_VOICE_ID || (existsSync(receiptPath)
  ? JSON.parse(readFileSync(receiptPath, "utf8")).voiceId
  : null);

const argv = process.argv.slice(2);
const text = argv.find((arg) => !arg.startsWith("--"));
const flag = (name) => {
  const index = argv.indexOf(`--${name}`);
  return index >= 0 ? argv[index + 1] : null;
};

if (!voiceId) throw new Error("Prutti voice has not been created; see voice.mjs");
if (!env.ELEVENLABS_API_KEY) throw new Error("ELEVENLABS_API_KEY is missing");
if (!text || !text.trim()) {
  throw new Error('usage: pruttivox.mjs "the text" [--from @handle] [--slug name] [--publish] [--force]');
}
if (text.length > 1200) {
  throw new Error(`text is ${text.length} characters; keep pruttivox clips under 1200`);
}

const slug = (flag("slug") || text.trim().toLowerCase()
  .replace(/[^\p{L}\p{N}\s-]/gu, "").split(/\s+/).slice(0, 5).join("-"))
  .replace(/[^a-z0-9æøå-]/g, "-").replace(/-+/g, "-").replace(/^-|-$/g, "");
if (!slug) throw new Error("could not derive a slug from the text; pass --slug");
const from = flag("from") || "";
const output = resolve(outRoot, `${slug}.mp3`);
const publish = argv.includes("--publish");

const log = existsSync(logPath)
  ? readFileSync(logPath, "utf8").trim().split("\n").filter(Boolean).map((line) => JSON.parse(line))
  : [];
if (publish && !argv.includes("--force") && log.some((entry) => entry.slug === slug && entry.published)) {
  throw new Error(`"${slug}" was already published; a republish sits stale on the CDN for ~1h — pick a new slug or pass --force`);
}

async function speak(line, destination) {
  const response = await fetch(`${apiBase}/v1/text-to-speech/${encodeURIComponent(voiceId)}`, {
    method: "POST",
    headers: { "xi-api-key": env.ELEVENLABS_API_KEY, "Content-Type": "application/json" },
    body: JSON.stringify({
      text: line,
      model_id: "eleven_multilingual_v2",
      voice_settings: { stability: 0.38, similarity_boost: 0.9, style: 0.48, use_speaker_boost: true, speed: 0.98 },
    }),
    signal: AbortSignal.timeout(120_000),
  });
  if (!response.ok) throw new Error(`ElevenLabs ${response.status}: ${(await response.text()).slice(0, 500)}`);
  writeFileSync(destination, Buffer.from(await response.arrayBuffer()), { mode: 0o600 });
}

mkdirSync(outRoot, { recursive: true });
if (!existsSync(tagPath)) await speak(tagText, tagPath);
const bodyPath = resolve(outRoot, `${slug}.body.mp3`);
await speak(text, bodyPath);

execFileSync("ffmpeg", [
  "-y", "-i", bodyPath, "-i", tagPath,
  "-filter_complex", "[0:a]apad=pad_dur=0.4[a0];[a0][1:a]concat=n=2:v=0:a=1",
  "-ar", "44100", "-b:a", "192k",
  "-metadata", "artist=Pruttivox (syntetisk stemme / synthetic voice)",
  "-metadata", "album=Klokkentales",
  "-metadata", `title=${slug}`,
  "-metadata", "comment=Consented ElevenLabs IVC — aesthetic.computer/klokkentales",
  output,
], { stdio: "ignore" });

const url = `https://assets.aesthetic.computer/${publishPrefix}/${slug}.mp3`;
if (publish) {
  execFileSync("aws", [
    "s3", "cp", output, `s3://assets-aesthetic-computer/${publishPrefix}/${slug}.mp3`,
    "--endpoint-url", "https://sfo3.digitaloceanspaces.com", "--acl", "public-read",
  ], { stdio: "inherit" });
}

mkdirSync(dirname(logPath), { recursive: true });
appendFileSync(logPath, JSON.stringify({
  slug, text, from, published: publish, url: publish ? url : undefined,
  at: new Date().toISOString(),
}) + "\n", { mode: 0o600 });

console.log(output);
if (publish) console.log(url);
